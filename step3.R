library(tidyverse)
library(taxize)
library(parallel)

biotime <- read_csv("biotime_results/biotime_common.csv") %>% 
  mutate(species_level = modify(parsed, function(x) {
    len <- str_split(x, " ", simplify = TRUE) %>% length()
    if (len == 1)
      FALSE 
    else 
      TRUE
  }) %>% as.logical()) %>%
  filter(species_level) %>% 
  select(-species_level)

tax <- read_rds("biotime_results/torino.rds")

sp <- biotime %>%
  left_join(tax) %>% 
  filter(is.na(matched)) %>% 
  pull(parsed) %>% 
  unique()

cl <- makeCluster(6)
res <- parSapply(
  cl,
  sp,
  function(x) taxize::gnr_resolve(x, with_context = TRUE)$matched_name[1]
)

# to be queried again -----
ans <- tibble(parsed = sp, resolved = res) %>% 
  mutate(resolved = modify(resolved, function(x) {
    paste(str_split(x, " ", simplify = TRUE)[1:2], collapse = " ")
  }) %>% unlist()) %>% 
  mutate(resolved = ifelse(resolved == "NA NA", parsed, resolved)) %>% 
  mutate(resolved = ifelse(grepl(" NA", resolved), parsed, resolved))

ans %>% 
  mutate(same = ifelse(parsed != resolved, FALSE, TRUE)) %>% 
  group_by(same) %>% 
  tally()

biotime %>% 
  left_join(
    ans %>% filter(parsed != resolved)
  ) %>% 
  filter(!is.na(resolved)) %>% 
  group_by(common) %>% 
  tally()

biotime <- biotime %>% 
  left_join(
    ans %>% filter(parsed != resolved)
  ) %>% 
  filter(!is.na(resolved))

# plants ------
plants <- biotime %>% 
  filter(common == "vascular plants") %>% 
  pull(resolved)

res_plants <- lcvplants::LCVP(plants)
res_plants <- tibble(original = res_plants$Submitted_Name,
                     LCVP = res_plants$LCVP_Accepted_Taxon,
                     score = res_plants$Score) %>% 
  mutate(LCVP = modify(LCVP, function(x) {
    paste(str_split(x, " ", simplify = TRUE)[1:2], collapse = " ")
  }) %>% unlist()) %>% 
  mutate(LCVP = ifelse(score != "matched", NA, LCVP))

# fishes ------
fishes <- biotime %>% 
  filter(common == "fishes") %>% 
  pull(resolved)

res_fishes <- parSapply(
  cl,
  fishes,
  function(x) rfishbase::validate_names(x)[1]
)
res_fishes <- tibble(original = names(res_fishes), fishbase = res_fishes)

res_fishes %>% 
  mutate(same = ifelse(original == fishbase, TRUE, FALSE)) %>%
  group_by(same) %>% tally()

# birds
birds <- biotime %>% 
  filter(common == "birds") %>% 
  pull(resolved)
new_tax <- rebird::ebirdtaxonomy()
res_birds <- parSapply(
  cl,
  birds,
  function(x) tryCatch(rebird::species_code(x),
                       error = function(e) NA)
)
res_birds <- tibble(parsed = birds, code = res_birds) %>%
  left_join(rebird:::tax %>%
              filter(speciesCode %in% res_birds) %>%
              transmute(code = speciesCode, ebird = sciName)) %>%
  select(-code)

res_birds <- res_birds %>% transmute(original = parsed, eBird = ebird)

res_birds %>% 
  mutate(same = ifelse(original == eBird, TRUE, FALSE)) %>%
  group_by(same) %>% tally()

# others
rest <-  biotime %>% 
  filter(is.na(common)) %>% 
  pull(resolved)

gbif <- parSapply(
  cl,
  rest,
  rgbif::name_backbone
)

res_other <- sapply(gbif, function(x) {
  if (x$matchType[1] == "NONE") {
    NA
  } else {
    x[1, "canonicalName"]
  }
})

res_other <- tibble(original = names(gbif), gbif = unlist(res_other)) %>% 
  mutate(gbif = modify(gbif, function(x) {
    a <- str_split(x, " ", simplify = TRUE)
    if (length(a) < 2) {
      NA
    } else {
      paste(a[1:2], collapse = " ")
    }
  }))
  
res_other %>% 
  mutate(same = ifelse(original == gbif, TRUE, FALSE)) %>% 
  group_by(same) %>% 
  tally()

stopCluster(cl)