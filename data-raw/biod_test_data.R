## code to prepare `biod` dataset goes here

library(ape)
library(tidyverse)

# Community data ----

# Read in community matrix exported from example Biodiverse project in Biodiverse v3.1
# example_project.bps (https://github.com/shawnlaffan/biodiverse/blob/master/data/example_project.bps)
# Menu: Basedata -> Export Groups -> ormat to use: Delimited text, save as "example_groups_exported.csv"

biod_comm_raw <- read_csv(here::here("data-raw/example_groups_exported.csv"))

# Convert to dataframe
biod_comm_df <-
	biod_comm_raw %>%
	# clean up column names (species)
	select(site = ELEMENT, contains("Genus")) %>%
	rename_with(~str_remove_all(., "Genus\\:")) %>%
	mutate(across(everything(), ~replace_na(., 0))) %>%
	column_to_rownames("site")

# Tree data ----

# Read in tree exported from example Biodiverse project
# example_project.bps (https://github.com/shawnlaffan/biodiverse/blob/master/data/example_project.bps)
# Menu: Tree -> Export tree -> Format to use: Newick, save as "example_tree_exported.tre"

tree_raw <- read_lines(here::here("data-raw/example_tree_exported.tre"))

biod_tree <-
	tree_raw %>%
	str_remove_all("Genus\\:") %>%
	paste0(";", collapse = "") %>%
	ape::read.tree(text = .)

# Generate Biodiverse analysis results ----

# Instructions are for Biodiverse v 3.1
# Open example_project.bps (https://github.com/shawnlaffan/biodiverse/blob/master/data/example_project.bps)
# For Basedata, select `example_data_x64`
# Create a new tree with branchlengths scaled to 1: Trees -> Rescale Branch Lengths -> save as `example_tree_RS1` with "new length" set to 1.0000000000 (default)
# Run analyses (following http://biodiverse-analysis-software.blogspot.com/2014/11/do-it-yourself-canape.html)
# - Name: example_data_x64_Spatial0
# - Neighbour set1: sp_self_only()
# - Neighbour set1: (nothing)
# - Calculations:
#    - Phylogenetic Endemism Indices
#        - Phylogenetic Endemism (PE_WE, PE_WE_P)
#    - Phylogenetic Indices
#        - Phylogenetic Diversity (PD, PD_P, PD_P_per_taxon, PD_per_taxon)
#    - Phylogenetic Indices (relative)
#        - Relative Phylogenetic Diversity, type 2 (PHYLO_RPD2, PHYLO_RPD_DIFF2, PHYLO_RPD_NULL2)
#        - Relative Phylogenetic Endemism, type 2 (PHYLO_RPE2, PHYLO_RPE_DIFF2, PHYLO_RPE_NULL2)
# Export results
# click "Export" button on left side -> "Delimited text"
# save file "example_spatial_analysis.csv"

# Read in biodiverse analysis results
biod_results <- read_csv(here::here("data-raw/example_spatial_analysis.csv")) %>%
	select(
		site = ELEMENT,
		pd_biodiv = PD_P,
		pd_alt_biodiv = PHYLO_RPD_NULL2,
		rpd_biodiv = PHYLO_RPD2,
		pe_biodiv = PE_WE_P,
		pe_alt_biodiv = PHYLO_RPE_NULL2,
		rpe_biodiv = PHYLO_RPE2
		)

# Write out data ----
biod_example <- list(
	phy = biod_tree,
	comm = biod_comm_df
)

usethis::use_data(biod_example, overwrite = TRUE)

usethis::use_data(biod_results, overwrite = TRUE)
