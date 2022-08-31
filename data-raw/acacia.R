## code to prepare `acacia` dataset goes here

library(janitor)
library(ape)
library(tidyverse)
library(assertr)
library(assertthat)
library(here)

# Load community ----

# Community data: Acacia community data from Mishler et al. 2014
# For more info, see data-raw/CANAPE_Acacia/README.md
acacia_comm <- read_csv(
  here("data-raw/CANAPE_Acacia/sites_by_spp.csv"),
  col_types = cols(.default = col_character())
) %>%
  clean_names() %>%
  select(-axis_0, -axis_1) %>%
  mutate(across(-element, as.numeric)) %>%
  mutate(across(-element, ~ replace_na(., 0))) %>%
  column_to_rownames("element")

# Load tree ----

# Need to do extra parsing of nexus file, since taxa names include spaces,
# parentheses, etc.
temp_phy <- tempfile("temp_phy.tre")

# Read in tree nexus file from treebase
acacia_nexus_raw <- readr::read_lines(
  here("data-raw/1_1363828941_Acacia.nexorg")
)

# The nexus file has one block for taxa names and
# one block for the tree.
# The tips of the tree are labeled with number codes, which
# correspond to taxon names.
# We want to relabel the tree so the tips are the taxon names, not numbers
acacia_nexus_raw %>%
  magrittr::extract(str_detect(., "TREE tree_1 = ")) %>%
  unlist() %>%
  str_remove_all("TREE tree_1 = ") %>%
  write_lines(temp_phy)

acacia_phy <- ape::read.tree(temp_phy)

fs::file_delete(temp_phy)

taxa_start <- acacia_nexus_raw %>%
  str_detect("TRANSLATE") %>%
  which() %>%
  magrittr::add(1)

taxa_end <- acacia_nexus_raw %>%
  str_detect("TREE tree_1 = ") %>%
  which() %>%
  magrittr::subtract(1)

# Write taxa block to temporary file to read in with read_tsv
temptable <- tempfile("table.tsv")

writeLines(acacia_nexus_raw[taxa_start:taxa_end], temptable)

# Make table of taxon number and name for all Acacia plus outgroups
acacia_with_og <- readr::read_tsv(
  temptable,
  col_names = FALSE, quote = "",
  col_types = cols(.default = col_character())
) %>%
  separate(X3, c("number", "name"), sep = " ", extra = "merge") %>%
  select(number, name)

unlink(temptable)

# Format table of ingroup taxa (named with specific epithet only)
acacia_taxa <-
  acacia_with_og %>%
  # drop outgroups
  filter(str_detect(name, "Acacia")) %>%
  mutate(name = str_replace_all(name, " ", "_")) %>%
  extract(name, "species", "(Acacia_[^_]+)", remove = FALSE) %>%
  mutate(
    species = str_remove_all(species, "Acacia_") %>%
      str_remove_all(",")
  ) %>%
  assert(not_na, species) %>%
  assert(is_uniq, species) %>%
  select(-name)

# Format table of ingroup taxa (named with genus and specific epithet)
og_taxa <-
  acacia_with_og %>%
  filter(str_detect(name, "Pararchidendron|Paraserianthes")) %>%
  mutate(name = str_replace_all(name, " ", "_")) %>%
  mutate(name = str_remove_all(name, "'")) %>%
  separate(name, c("genus", "epithet"), sep = "_", extra = "drop") %>%
  unite("species", genus, epithet)

# Fix one label to match between comm and phy:
# 'clunies' in comm, but 'clunies-rossiae' in phy
acacia_taxa$species[acacia_taxa$species == "clunies-rossiae"] <- "clunies"

# Make tibble of tip labels matching numbers in tree
new_tips <-
  tibble(number = acacia_phy$tip.label) %>%
  left_join(bind_rows(acacia_taxa, og_taxa), by = "number") %>%
  assert(not_na, number) %>%
  assert(is_uniq, number, species)

acacia_phy$tip.label <- new_tips$species

acacia <- list(
  phy = acacia_phy,
  comm = acacia_comm
)

# Final checks
# - should be 510 tips in tree
assert_that((Ntip(acacia$phy) == 510))
# - All names besides outgroups should match
assert_that(
  isTRUE(
    all.equal(
      sort(colnames(acacia_comm)),
      sort(drop.tip(acacia$phy, og_taxa$species)$tip.label)
    )
  )
)

usethis::use_data(acacia, overwrite = TRUE)
