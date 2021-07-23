library(tidyverse)
library(magrittr)

# I know I could call read_csv instead of read.csv %>% as_tibble, but it 
# was broken when I wrote this.

make_binomial <- function(x) {
    splitted <- str_split(x, " ", simplify = TRUE)
    if (length(splitted) < 2) {
      ans <- NA
    } else {
      ans <- paste(splitted[1], splitted[2], collapse = " ")
    }
    return(ans)
}

setwd("biotime_results")

# Bogota -------------
bogota <- read_csv("biotime_common.csv") %>%
  select(-class, -phylum) %>%
  left_join(read_csv("bogota_ebird.csv") %>% 
              distinct_all(),
            by = "parsed") %>% 
  left_join(read_csv("bogota_fishbase.csv") %>% 
              distinct_all(),
            by = "parsed") %>% 
  left_join(read_csv("bogota_lcvp.csv") %>% 
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

# Torino -------
torino <- read_csv("biotime_common.csv") %>%
  select(-class, -phylum) %>%
  left_join(read_csv("torino_ebird.csv") %>% 
              distinct_all(),
            by = "parsed") %>% 
  left_join(read_csv("torino_fishbase.csv") %>% 
              distinct_all(),
            by = "parsed") %>% 
  left_join(read_csv("torino_lcvp.csv") %>% 
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

# GBIF pre-processed ----------------
gbif_preproc <- read_csv("biotime_common.csv") %>%
  left_join(read_csv("gbif_preproc.csv") %>%
              transmute(parsed, matched = gbif) %>% 
              distinct_all())

# GBIF only ----------------
gbif <-  read_csv("biotime_common.csv") %>%
  left_join(read_csv("gbif_only.csv") %>%  #I call this as it is the same
              transmute(BioTIME = biotime, matched = gbif) %>% 
              distinct_all())

# keep only binomial names -------------
torino %<>% mutate(binomial = modify(matched, make_binomial))
bogota %<>% mutate(binomial = modify(matched, make_binomial))
gbif_preproc %<>% mutate(binomial = modify(matched, make_binomial))
gbif %<>% mutate(binomial = modify(matched, make_binomial))

# write to RDS file -----------
write_rds(list(bogota = bogota,
               torino = torino,
               gbif_preproc = gbif_preproc,
               gbif = gbif),
          "harmonized.rds")
