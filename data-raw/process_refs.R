# Filter a list of references in YAML format to those cited in an RMD file
#
# The YAML list should be exported from Zotero
# like this: file -> "export library" -> "Better CSL YAML"

library(tidyverse)
library(yaml)
library(assertr)

filter_refs <- function(rmd, yaml_in, yaml_out) {

# Set path to RMD file
rmd_file <- here::here(rmd)

# Parse RMD file and extract citation keys
citations <-
  readr::read_lines(rmd_file) %>%
  stringr::str_split(" |;") %>%
  unlist() %>%
  magrittr::extract(., stringr::str_detect(., "@")) %>%
  stringr::str_remove_all("\\[|\\]|\\)|\\(|\\.$|,|\\{|\\}") %>%
  magrittr::extract(., stringr::str_detect(., "^@|^-@")) %>%
  stringr::str_remove_all("^@|^-@") %>%
  unique() %>%
  sort() %>%
  tibble(key = .)

# Read in YAML including all references exported from Zotero
ref_yaml <- yaml::read_yaml(yaml_in)

# Extract citation keys from YAML, filter to only those in the RMD
cite_keys <- map_chr(ref_yaml$references, "id") %>%
  tibble(
    key = .,
    order = 1:length(.)
  ) %>%
  inner_join(citations, by = "key") %>%
  # Stop if any keys are missing
  assert(not_na, everything())

# Filter YAML to only those citation keys in the RMD
list(references = ref_yaml$references[cite_keys$order]) %>%
  # Write out the YAML file
  yaml::write_yaml(file = yaml_out)
}

filter_refs(
  "vignettes/canape.Rmd",
  here::here("data-raw/main_library.yaml"),
  file = here::here("vignettes/references.yaml")
  )

filter_refs(
  "vignettes/nrand.Rmd",
  here::here("data-raw/main_library.yaml"),
  here::here("vignettes/nrand_references.yaml")
)
