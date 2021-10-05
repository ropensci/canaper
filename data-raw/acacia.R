## code to prepare `acacia` dataset goes here

library(janitor)
library(ape)
library(tidyverse)
library(assertr)
library(here)

# Load community ----

# acacia_sites_by_spp.csv downloaded from github
# https://github.com/shawnlaffan/biodiverse/tree/73522b74e52a5fb77ae5cfc2e90010350a3abf70/etc/experiments/independent_swaps
acacia_comm <- read_csv(here("data-raw/acacia_sites_by_spp.csv"), col_types = cols(.default = col_character())) %>%
  clean_names() %>%
  select(-axis_0, -axis_1) %>%
  mutate(across(-element, as.numeric)) %>%
  mutate(across(-element, ~ replace_na(., 0))) %>%
  column_to_rownames("element")

# Load tree ----

# Need to do extra parsing of nexus file, since taxa names include spaces, parentheses, etc.
temp_phy <- tempfile("temp_phy.tre")

acacia_nexus_raw <- readr::read_lines(here("data-raw/1_1363828941_Acacia.nexorg"))

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

acacia_taxa <- acacia_nexus_raw[taxa_start:taxa_end] %>%
  readr::read_table(col_names = FALSE) %>%
  separate(X1, c("number", "name"), sep = " ", extra = "merge") %>%
  filter(str_detect(name, "Acacia")) %>%
  mutate(name = str_replace_all(name, " ", "_")) %>%
  extract(name, "species", "(Acacia_[^_]+)", remove = FALSE) %>%
  mutate(species = str_remove_all(species, "Acacia_") %>% str_remove_all(",")) %>%
  assert(not_na, species) %>%
  assert(is_uniq, species) %>%
  select(-name)

og_taxa <- acacia_nexus_raw[taxa_start:taxa_end] %>%
  readr::read_table(col_names = FALSE) %>%
  separate(X1, c("number", "name"), sep = " ", extra = "merge") %>%
  filter(str_detect(name, "Pararchidendron|Paraserianthes")) %>%
  mutate(name = str_replace_all(name, " ", "_")) %>%
  mutate(name = str_remove_all(name, "'")) %>%
  separate(name, c("genus", "epithet"), sep = "_", extra = "drop") %>%
  unite("species", genus, epithet)

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

usethis::use_data(acacia, overwrite = TRUE)
