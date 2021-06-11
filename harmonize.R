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

biotime <- read_csv("~/Documents/databases/biotime.txt", col_names = "BioTIME")
biotime %<>% mutate(parsed = gn_parse_tidy(BioTIME)$canonicalsimple)

#============================
# TORINO PIPELINE
#============================

# get taxonomic class
cl <- makeCluster(6)
classes <- parSapply(
  cl,
  biotime$parsed,
  function(x) rgbif::name_backbone(x, strict = FALSE)$class #no fuzzy
)
stopCluster(cl)
classes <- tibble(parsed = names(classes), class = classes) %>%
  unnest(cols = class)
biotime %<>% left_join(classes %>% distinct_all())

# get phylum
phylum <- parSapply(
  cl,
  biotime %>% 
    pull(class) %>% 
    unique(),
  function(x) rgbif::name_backbone(x, strict = FALSE)$phylum #no fuzzy
)
phylum <- tibble(class = names(phylum), phylum = phylum) %>% 
  unnest(cols = phylum)
biotime %<>% left_join(phylum %>% distinct_all())

biotime %<>% left_join(
  tibble(class = phylum$class,
         matched_class = sapply(phylum$class, assign_group),
         matched_phylum = sapply(phylum$phylum, assign_group)) %>%
  filter(!is.na(matched_class) | !is.na(matched_phylum)) %>% 
  transmute(class, common = modify2(matched_class, matched_phylum,
                                    function(x, y) {
                                       ifelse(is.na(x), y, x)
                                    }))
  )
write_csv(biotime, "~/Documents/biotime_common.csv")
stopCluster(cl)

# harmonizing against databases ---------------------
d <- read_csv("~/Documents/biotime_common.csv")

# vascular plants
plants <- d %>% 
  filter(common == "vascular plants") %>% 
  pull(parsed)
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
   write_csv("~/Documents/torino_lcvp.csv")

# fishes
fishes <- d %>% 
  filter(common == "fishes") %>%
  select(parsed)
cl <- makeCluster(6)
fishbase <- parSapply(
   cl,
   fishes$parsed,
   function(x) rfishbase::validate_names(x)[1]
)
fishbase <- fishes %>% mutate(fishbase = fishbase)
write_csv(fishbase, "~/Documents/torino_fishbase")

# birds
birds <- d %>%
   filter(common == "birds") %>%
   pull(parsed)
new_tax <- ebirdtaxonomy()
ebird <- parSapply(
    cl,
    birds,
    function(x) tryCatch(rebird::species_code(x),
                         error = function(e) NA)
)
birds <- tibble(parsed = birds, ebird = ebird)
write_csv(birds, "~/Documents/torino_ebird.csv")

# rest with GBIF
unmatched <- biotime %>%
  filter(common %out% c("vascular plants", "fishes", "birds")) %>%
  pull(parsed)
gbif <- parSapply(
  cl,
  unmatched,
  name_backbone
)
stopCluster(cl)
bind_rows(gbif) %>%
   mutate(parsed = unmatched,
          common = modify(class, assign_group)) %>%
   transmute(parsed, gbif = canonicalName) %>%
   write_csv("~/Documents/torino_gbif.csv")

# GBIF + combine results --------------------
biotime <- read_csv("~/Documents/biotime_common.csv")
plants <- read_csv("~/Documents/torino_lcvp.csv")
fishes <- read_csv("~/Documents/torino_fishbase.csv")
birds <- read_csv("~/Documents/torino_ebird.csv")
gbif <- read_csv("~/Documents/torino_gbif.csv")

res <- biotime %>%  
  select(-class, -phylum, -BioTIME) %>%
  distinct_all() %>%
  left_join(plants %>% distinct_all()) %>%
  left_join(fishes %>% distinct_all()) %>%
  left_join(birds %>% distinct_all()) %>%
  left_join(gbif %>% distinct_all()) %>%
  mutate(match = pmap(list(lcvp, fishbase, ebird, gbif), function(x, y, z, w) {
    if (!is.na(x))
       "LCVP"
    else if (!is.na(y))
       "FishBase"
    else if (!is.na(z))
       "eBird"
    else if (!is.na(w))
       "GBIF"
    else
       NA
    }) %>% unlist()) %>%
  select(-lcvp, -fishbase, -ebird, -gbif) %>%
  mutate(matched = ifelse(is.na(match), FALSE, TRUE)) %>%
  distinct_all() %>%
  group_by(match, common) %>%
  tally() %>%
  pivot_wider(names_from = match, values_from = n) %>%
  mutate(unmatched = `NA`) %>%
  select(-`NA`)

res %>% 
   pivot_longer(cols = 2:6) %>%
   pull(value) %>%
   sum(na.rm = TRUE)
length(unique(biotime$parsed))

res %>% write_csv("~/Documents/torino_final.csv")

#============================
# BOGOTA PIPELINE
#============================
d <- read_csv("~/Documents/biotime_common.csv") %>%
   select(-class, -phylum, -common)
cl <- makeCluster(6)
lcvp <- parLapply(
   cl,
   d$parsed,
   function(x) {
      tryCatch(suppressMessages(lcvplants::LCVP(x)),
               error = function(e) return(NA))
   }
)
lcvp <- bind_rows(lcvp[which(!is.na(lcvp))])
write_csv(lcvp, "~/Documents/bogota_lcvp.csv")
fishbase <- parSapply(
   cl,
   fishes$parsed,
   function(x) rfishbase::validate_names(x)[1]
)
stopCluster(cl)
#============================
# GBIF PIPELINE
#============================

d <- read_csv("~/Documents/biotime_common.csv")
plants <- d %>% 
  filter(common == "vascular plants") %>% 
  pull(parsed)
fishes <- d %>% 
  filter(common == "fishes") %>%
  select(parsed)
plants <- tibble(parsed = plants,
                 GBIF = name_backbone(plants))
fishes <- tibble(parsed = fishes,
                 GBIF = name_backbone(fishes))
