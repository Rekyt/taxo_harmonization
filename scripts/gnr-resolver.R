library(tidyverse)
library(magrittr)
library(rgnparser)
library(rgbif)
library(lcvplants)
library(rfishbase)
library(rebird)
library(parallel)

"%out%" <- Negate("%in%")
assign_group <- function(x){
  switch (x,
          "Tracheophyta" = "vascular plants",
          "Aves" = "birds",
          "Mammalia" = "mammals",
          "Chondrichthyes" = "fishes",
          "Actinopterygii" = "fishes",
          "Sarcopterygii" = "fishes",
          "Elasmobranchii" = "fishes",
          "Cephalaspidomorphi" = "fishes",
          "Myxini" = "fishes",
          "Holocephali" = "fishes",
          NA)
}

# get unmatched species and run gnr_resolve -----
d <- read_rds("biotime_results/harmonized.rds")
d <- d$torino %>% 
  filter(!is.na(common), is.na(matched), common != "mammals") %>% 
  distinct_all()

d <- d %>% 
  filter(is.na(matched)) %>%
  mutate(species_level = modify(parsed, function(x) {
    if(ncol(str_split(x, " ", simplify = TRUE)) < 2) {
      FALSE
    } else {
      TRUE
    }
  })) %>% 
  mutate(species_level = as.logical(species_level)) %>% 
  filter(species_level) %>% 
  select(-species_level) %>% 
  distinct(parsed, .keep_all = TRUE)

d %>%
  group_by(common) %>% 
  tally()
  
gnr <- taxize::gnr_resolve(d$parsed, with_context = TRUE) 

write_csv(gnr, "biotime_results/gnrparser.csv")
write_csv(d, "biotime_results/wf2-gnr.csv")

# gnr already ran ------
gnr <- read_csv("biotime_results/gnrparser.csv")
d <- read_csv("biotime_results/wf2-gnr.csv")
d <- d %>% left_join(
  gnr %>% 
    transmute(parsed = user_supplied_name,
              gnr = matched_name) %>% 
    group_by(parsed) %>% 
    slice_head(n = 1) %>% 
    ungroup()
) %>% 
  mutate(gnr = modify(gnr, function(x) {
    paste(str_split(x, " ", simplify = TRUE)[1:2], collapse = " ")
  })) %>% 
  select(parsed, gnr, common) %>% 
  filter(parsed != gnr)

d %>% 
  filter(!grepl("NA", gnr)) %>% 
  nrow()

d <- d %>% 
  filter(!grepl("NA", gnr)) %>% 
  distinct(gnr, .keep_all = TRUE)

cl <- makeCluster(4)

# plants --------
plants <- d %>% 
  filter(common == "vascular plants") %>% 
  pull(gnr)
lcvp <- LCVP(plants)
lcvp %>%
  tibble() %>%
  transmute(parsed = Submitted_Name,
            lcvp = LCVP_Accepted_Taxon) %>%
  distinct_all() %>%
  right_join(tibble(parsed = unique(plants))) %>%
  distinct_all() %>%
  arrange(parsed, lcvp) %>%
  group_by(parsed) %>%
  slice(1) %>%
  ungroup() %>% 
  filter(lcvp != "", !is.na(lcvp)) %>% 
  distinct_all()

# fishes ---------
fishes <- d %>% 
  filter(common == "fishes") %>%
  pull(gnr)
fishbase <- parSapply(
  cl,
  fishes,
  function(x) rfishbase::validate_names(x)[1]
)

sum(!is.na(fishbase))

# birds ------
birds <- d %>%
  filter(common == "birds") %>%
  pull(gnr)
new_tax <- ebirdtaxonomy()
ebird <- parSapply(
  cl,
  birds,
  function(x) tryCatch(rebird::species_code(x),
                       error = function(e) NA)
)
birds <- tibble(parsed = birds,
                code = ebird) %>%
  left_join(rebird:::tax %>%
              filter(speciesCode %in% ebird) %>%
              transmute(code = speciesCode, ebird = sciName)) %>%
  select(-code)
birds %>% 
  filter(!is.na(ebird)) %>% 
  distinct_all()
