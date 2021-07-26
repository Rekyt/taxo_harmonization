# Script to write result files to merge dataset before quantitative comparison
# Author: Emilio Berti

# Packages ---------------------------------------------------------------------
library("tidyverse")
library("magrittr")

# Functions --------------------------------------------------------------------
make_binomial <- function(x) {
  splitted <- str_split(x, " ", simplify = TRUE)
  if (length(splitted) < 2) {
    ans <- NA
  } else {
    ans <- paste(splitted[1], splitted[2], collapse = " ")
  }
  return(ans)
}


# Merge results from Workflow 1 (Torino = high tax. group) ---------------------

torino <- read_csv("data/data_cleaned/biotime_results/biotime_common.csv") %>%
  select(-class, -phylum) %>%
  left_join(read_csv("data/data_cleaned/biotime_results/torino_ebird.csv") %>% 
              distinct_all(),
            by = "parsed") %>% 
  left_join(
    read_csv("data/data_cleaned/biotime_results/torino_fishbase.csv") %>% 
      distinct_all(),
    by = "parsed") %>% 
  left_join(read_csv("data/data_cleaned/biotime_results/torino_lcvp.csv") %>% 
              distinct_all() %>% 
              mutate(lcvp = modify(lcvp, function(x)
                ifelse(x == "unresolved", NA, x))),
            by = "parsed") %>% 
  mutate(lcvp = ifelse(lcvp == "", NA, lcvp))

torino <- torino %>% 
  mutate(matched = pmap(list(ebird, fishbase, lcvp), function(x, y, z) {
    choices <- c(x, y ,z)
    if (all(is.na(choices))) {
      NA
    } else {
      valid <- which(!is.na(choices))
      if (length(valid) > 1) { #multiple matches - error!
        NA
      } else {
        c(x, y, z)[valid]
      }
    }
  }) %>% unlist())


# Merge results from Workflow 2 (Bogota, all list against dbs) -----------------

bogota <- read_csv("data/data_cleaned/biotime_results/biotime_common.csv") %>%
  select(-class, -phylum) %>%
  left_join(read_csv("data/data_cleaned/biotime_results/bogota_ebird.csv") %>% 
              distinct_all(),
            by = "parsed") %>% 
  left_join(
    read_csv("data/data_cleaned/biotime_results/bogota_fishbase.csv") %>% 
      distinct_all(),
    by = "parsed") %>% 
  left_join(read_csv("data/data_cleaned/biotime_results/bogota_lcvp.csv") %>% 
              distinct_all() %>% 
              mutate(lcvp = modify(lcvp, function(x)
                ifelse(x == "unresolved", NA, x))),
            by = "parsed") %>% 
  mutate(lcvp = ifelse(lcvp == "", NA, lcvp))

bogota <- bogota %>% 
  mutate(matched = pmap(list(ebird, fishbase, lcvp), function(x, y, z) {
    choices <- c(x, y ,z)
    if (all(is.na(choices))) {
      NA
    } else {
      valid <- which(!is.na(choices))
      if (length(valid) > 1) { #multiple matches - error!
        NA
      } else {
        c(x, y, z)[valid]
      }
    }
  }) %>% unlist())


# Merge results from workflow 3 (GBIF only pre-processed) ----------------------
gbif_preproc <- read_csv(
  "data/data_cleaned/biotime_results/biotime_common.csv"
  ) %>%
  left_join(
    read_csv("data/data_cleaned/biotime_results/gbif_preproc.csv") %>%
      transmute(parsed, matched = gbif) %>% 
      distinct_all()
  )


# Merge results from workflow 4 (GBIF only no pre-processing) ------------------

gbif <-  read_csv("data/data_cleaned/biotime_results/biotime_common.csv") %>%
  left_join(read_csv("data/data_cleaned/biotime_results/gbif_only.csv") %>%
              transmute(BioTIME = biotime, matched = gbif) %>% 
              distinct_all())


# Keep only binomial names -----------------------------------------------------

torino %<>% mutate(binomial = modify(matched, make_binomial))
bogota %<>% mutate(binomial = modify(matched, make_binomial))
gbif_preproc %<>% mutate(binomial = modify(matched, make_binomial))
gbif %<>% mutate(binomial = modify(matched, make_binomial))


# Write final results to RDS file ----------------------------------------------
write_rds(list(bogota       = bogota,
               torino       = torino,
               gbif_preproc = gbif_preproc,
               gbif         = gbif),
          "data/data_cleaned/biotime_results/harmonized.rds")
