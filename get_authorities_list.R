# Script to extract a list of taxonomic authorities accessed by the packages
# included in the table

# Packages ---------------------------------------------------------------------
library("dplyr")

# Load data --------------------------------------------------------------------

# Raw data from the Google Doc sheets exported as XLSX file
raw_pkg_table = readxl::read_xlsx("data_raw/Table comparing taxonomic tools.xlsx",
                                  na = c("", "NA"))

# Get the list of packages that are included in the review
included_pkg = raw_pkg_table %>%
  filter(`Should we include this package in our review?` == "include")

# Get authorities list ---------------------------------------------------------
authorities = included_pkg %>%
  pull(`Which authority?`) %>%
  unique() %>%
  na.omit() %>%
  as.character() %>%
  paste(collapse = ",") %>%
  strsplit(",", fixed = TRUE) %>%
  lapply(trimws) %>%
  .[[1]] %>%
  unique()

# Save into a csv --------------------------------------------------------------
authorities %>%
  tibble::as_tibble() %>%
  readr::write_csv("data_cleaned/authorities_list.csv")
