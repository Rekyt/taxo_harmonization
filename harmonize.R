# Script to harmonize taxonomy in BioTIME
# Author: Emilio Berti


# Needed Packages --------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(rgnparser)
library(rgbif)
library(lcvplants)
library(rfishbase)
library(rebird)
library(parallel)


# Functions --------------------------------------------------------------------

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


# Load data --------------------------------------------------------------------

biotime <- read_csv("~/Documents/databases/biotime.txt",
                    col_names = "BioTIME") %>%
  mutate(BioTIME = gsub("Family ", "", BioTIME),
         BioTIME = gsub("Order ", "", BioTIME),
         BioTIME = gsub("Suborder ", "", BioTIME),
         BioTIME = gsub("Genus ", "", BioTIME),
         BioTIME = gsub("Phylum ", "", BioTIME),
         BioTIME = gsub("Subphylum ", "", BioTIME),
         BioTIME = gsub("Superfamily ", "", BioTIME),
         BioTIME = gsub("Subclass ", "", BioTIME),
         BioTIME = gsub("Class ", "", BioTIME))

biotime %<>% mutate(parsed = gn_parse_tidy(BioTIME)$canonicalsimple)

cl <- makeCluster(5) #parallize operation thought the whole script


# TORINO Step 1 --------------------------------------------------------------
# Get taxonomic groups

message(" --- TORINO ---")

# get taxonomic class
message("     GBIF taxonomic group")
classes <- parSapply(
  cl,
  biotime$parsed,
  function(x) rgbif::name_backbone(x, strict = FALSE)$class #no fuzzy
)
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


# TORINO Step 2 ----------------------------------------------------------------
# Harmonize against each database depending on identified group

d <- read_csv("~/Documents/biotime_common.csv")

# vascular plants
message("     LCVP")
T0 <- Sys.time()
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
message("     FishBase")
fishes <- d %>% 
  filter(common == "fishes") %>%
  select(parsed)
fishbase <- parSapply(
   cl,
   fishes$parsed,
   function(x) rfishbase::validate_names(x)[1]
)
fishbase <- fishes %>% mutate(fishbase = fishbase)
write_csv(fishbase, "~/Documents/torino_fishbase")
message("     ", Sys.time() - T0)

# birds
message("     eBird")
T <- Sys.time()
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
birds <- tibble(parsed = birds,
                code = ebird) %>%
  left_join(rebird:::tax %>%
              filter(speciesCode %in% ebird) %>%
              transmute(code = speciesCode, ebird = sciName)) %>%
  select(-code)
write_csv(birds, "~/Documents/torino_ebird.csv")
message("     ", Sys.time() - T0)

# rest with GBIF
message("     GBIF")
T0 <- Sys.time()
unmatched <- biotime %>%
  filter(common %out% c("vascular plants", "fishes", "birds")) %>%
  pull(parsed)
gbif <- parSapply(
  cl,
  unmatched,
  name_backbone
)
bind_rows(gbif) %>%
   mutate(parsed = unmatched,
          common = modify(class, assign_group)) %>%
   transmute(parsed, gbif = canonicalName) %>%
   write_csv("~/Documents/torino_gbif.csv")
message("     ", Sys.time() - T0)


# TORINO Step 3 ----------------------------------------------------------------
# Combine results

biotime <- read_csv("~/Documents/biotime_common.csv") %>%
  mutate(species_level = modify(parsed, function(x) {
      len <- str_split(x, " ", simplify = TRUE) %>% length()
      if (len == 1)
        FALSE 
      else 
        TRUE
    }) %>% as.logical()) %>%
  filter(species_level) %>%
  select(-species_level)
plants <- read_csv("~/Documents/torino_lcvp.csv")
fishes <- read_csv("~/Documents/torino_fishbase.csv")
birds <- read_csv("~/Documents/torino_ebird.csv")
gbif <- read_csv("~/Documents/torino_gbif.csv") %>%
  mutate(species_level = modify(gbif, function(x) {
      len <- str_split(x, " ", simplify = TRUE) %>% length()
      if (len == 1)
        FALSE 
      else 
        TRUE
    }) %>% as.logical()) %>%
  filter(species_level) %>%
  select(-species_level)

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


# BOGOTA Step 1 ----------------------------------------------------------------
message(" --- BOGOTA")
d <- read_csv("~/Documents/biotime_common.csv") %>%
   select(-class, -phylum, -common)  # not needed in this pipeline

# Against LCVP
message("     LCVP")
T0 <- Sys.time()
lcvp <- parLapply(
   cl,
   d$parsed,
   function(x) {
      tryCatch(suppressMessages(lcvplants::LCVP(x)),
               error = function(e) return(NA))
   }
)
lcvp <- bind_rows(lcvp[which(!is.na(lcvp))])
lcvp %>%
   tibble() %>%
   transmute(parsed = Submitted_Name,
             lcvp = LCVP_Accepted_Taxon) %>%
   distinct_all() %>%
   arrange(parsed, lcvp) %>%
   group_by(parsed) %>%
   slice(1) %>%
   ungroup() %>%
   write_csv("~/Documents/bogota_lcvp.csv")
message("     ", Sys.time() - T0)

# Against FishBase
message("     FishBase")
T0 <- Sys.time()
fishbase <- parSapply(
   cl,
   d$parsed,
   function(x) rfishbase::validate_names(x)[1]
)
fishbase <- tibble(parsed = d$parsed, fishbase = fishbase)
write_csv(fishbase, "~/Documents/bogota_fishbase.csv")
message("     ", Sys.time() - T0)

# Against eBird
message("     ebird")
T0 <- Sys.time()
ebird <- parSapply(
    cl,
    d$parsed,
    function(x) tryCatch(rebird::species_code(x),
                         error = function(e) NA)
)
birds <- tibble(parsed = d$parsed,
                code = ebird) %>%
  left_join(rebird:::tax %>%
              filter(speciesCode %in% ebird) %>%
              transmute(code = speciesCode, ebird = sciName)) %>%
  select(-code)
write_csv(birds, "~/Documents/bogota_ebird.csv")
message("     ", Sys.time() - T0)


# BOGOTA Step 2 ----------------------------------------------------------------
# Combine results

biotime <- read_csv("~/Documents/biotime_common.csv") %>%
  mutate(species_level = modify(parsed, function(x) {
      len <- str_split(x, " ", simplify = TRUE) %>% length()
      if (len == 1)
        FALSE 
      else 
        TRUE
    }) %>% as.logical()) %>%
  filter(species_level) %>%
  select(-species_level)
plants <- read_csv("~/Documents/bogota_lcvp.csv")
fishes <- read_csv("~/Documents/bogota_fishbase.csv")
birds <- read_csv("~/Documents/bogota_ebird.csv")

res <- biotime %>%  
  select(-class, -phylum, -BioTIME) %>%
  distinct_all() %>%
  left_join(plants %>% distinct_all()) %>%
  left_join(fishes %>% distinct_all()) %>%
  left_join(birds %>% distinct_all())
# resolve conflicts
res <- res %>%
  mutate(conflict = pmap(list(lcvp, fishbase, ebird), 
                         function(x, y, z) {
                           valid <- !is.na(c(x, y, z))
                           if (sum(valid) > 1)
                             TRUE
                           else
                             FALSE
                         }) %>% unlist() %>% as.logical()) %>%
  filter(!conflict) %>%
  select(-conflict)
# summary
res %>%
  mutate(match = pmap(list(lcvp, fishbase, ebird), 
                      function(x, y, z) {
                        if (!is.na(x))
                          "LCVP"
                        else if (!is.na(y))
                          "FishBase"
                        else if (!is.na(z))
                          "eBird"
                        else
                          NA
                        }) %>% unlist()) %>%
  mutate(matched = ifelse(is.na(match), FALSE, TRUE)) %>%
  select(-lcvp, -fishbase, -ebird) %>%
  distinct_all() %>%
  group_by(match, common) %>%
  tally() %>%
  pivot_wider(names_from = match, values_from = n) %>%
  mutate(unmatched = `NA`) %>%
  select(-`NA`)

res %>%
  filter(common != "vascular plants", !is.na(lcvp))

res %>% 
   pivot_longer(cols = 2:5) %>%
   pull(value) %>%
   sum(na.rm = TRUE)
length(unique(biotime$parsed))

res %>% write_csv("~/Documents/bogota_final.csv")


# GBIF only --------------------------------------------------------------------
# Match names only with GBIF taxonomic backbone

d <- read_csv("~/Documents/biotime_common.csv") %>%
   select(-class, -phylum, -common) #not needed in this pipeline
gbif <- parSapply(
  cl,
  d$parsed,
  function(x) {
    ans <- rgbif::name_backbone(x, strict = FALSE) #no fuzzy
    if ("canonicalName" %in% names(ans))
      return(ans$canonicalName)
    else
      return(NA)
  }
)
tibble(parsed = d$parsed, gbif = unlist(gbif)) %>%
   write_csv("~/Documents/bogota_gbif.csv")

stopCluster(cl)

biotime <- read_csv("~/Documents/biotime_common.csv")
gbif <- read_csv("~/Documents/bogota_gbif.csv")
gbif %>%
  mutate(species_level = modify(gbif, function(x) {
      len <- str_split(x, " ", simplify = TRUE) %>% length()
      if (len == 1)
        FALSE 
      else 
        TRUE
    }) %>% as.logical()) %>%
  filter(species_level) %>%
  left_join(biotime) %>%
  select(-BioTIME) %>%
  distinct_all() %>%
  group_by(common) %>%
  filter(!is.na(gbif)) %>%
  tally()


# Workflow Comparison ----------------------------------------------------------

biotime <- read_csv("~/Documents/biotime_common.csv") %>%
  mutate(species_level = modify(parsed, function(x) {
      len <- str_split(x, " ", simplify = TRUE) %>% length()
      if (len == 1)
        FALSE 
      else 
        TRUE
    }) %>% as.logical()) %>%
  distinct(parsed, .keep_all = TRUE) %>%
  filter(species_level) %>%
  select(-species_level)
# workflow 1 -------
plants <- read_csv("~/Documents/bogota_lcvp.csv")
fishes <- read_csv("~/Documents/bogota_fishbase.csv")
birds <- read_csv("~/Documents/bogota_ebird.csv")
gbif <- read_csv("~/Documents/bogota_gbif.csv")
gbif %>%
  mutate(species_level = modify(gbif, function(x) {
      len <- str_split(x, " ", simplify = TRUE) %>% length()
      if (len == 1)
        FALSE 
      else 
        TRUE
    }) %>% as.logical()) %>%
  filter(species_level) %>%
  left_join(biotime) %>%
  select(-BioTIME) %>%
  distinct_all() %>%
  group_by(common) %>%
  filter(!is.na(gbif)) %>%
  tally()
wf1 <- biotime %>%  
  select(-class, -phylum, -BioTIME) %>%
  distinct_all() %>%
  left_join(plants %>% distinct_all()) %>%
  left_join(fishes %>% distinct_all()) %>%
  left_join(birds %>% distinct_all()) %>%
  left_join(gbif %>% distinct_all())
#remove GBIF if another db found something
wf1 <- wf1 %>%
  mutate(remove_gbif = pmap(list(lcvp, fishbase, ebird), 
                         function(x, y, z) {
                           valid <- !is.na(c(x, y, z))
                           if (any(valid))
                             TRUE
                           else 
                             FALSE
                         }) %>% unlist() %>% as.logical()) %>%
  mutate(gbif = modify2(gbif, remove_gbif, function(x, y) {
      if (y)
        NA
      else
        x
    })) %>% 
  select(-remove_gbif)
wf1 <- wf1 %>%
  mutate(conflict = pmap(list(lcvp, fishbase, ebird), 
                         function(x, y, z) {
                           valid <- !is.na(c(x, y, z))
                           if (sum(valid) > 1)
                             TRUE
                           else
                             FALSE
                         }) %>% unlist() %>% as.logical()) %>%
  filter(!conflict) %>%
  select(-conflict)

# workflow 2 ----------
plants <- read_csv("~/Documents/torino_lcvp.csv")
fishes <- read_csv("~/Documents/torino_fishbase.csv")
birds <- read_csv("~/Documents/torino_ebird.csv")
gbif <- read_csv("~/Documents/torino_gbif.csv") %>%
  mutate(species_level = modify(gbif, function(x) {
      len <- str_split(x, " ", simplify = TRUE) %>% length()
      if (len == 1)
        FALSE 
      else 
        TRUE
    }) %>% as.logical()) %>%
  filter(species_level) %>%
  select(-species_level)
wf2 <- biotime %>%  
  select(-class, -phylum, -BioTIME) %>%
  distinct_all() %>%
  left_join(plants %>% distinct_all()) %>%
  left_join(fishes %>% distinct_all()) %>%
  left_join(birds %>% distinct_all()) %>%
  left_join(gbif %>% distinct_all())

# harmonized ----------
wf1 <- wf1 %>%
  select(-common) %>%
  pivot_longer(cols = 2:5, names_to = "step", values_to = "matched") %>%
  filter(!is.na(matched)) %>%
  select(-step)

wf2 <- wf2 %>%
  select(-common) %>%
  pivot_longer(cols = 2:5, names_to = "step", values_to = "matched") %>%
  filter(!is.na(matched)) %>%
  select(-step)

gbif

wf1 %>%
  mutate(matched = modify(matched, function(x) {
      paste(str_split(x, " ", simplify = TRUE)[1:2], collapse = " ")
    })) %>%
  full_join(wf2 %>%
              mutate(matched = modify(matched, function(x) {
                paste(str_split(x, " ", simplify = TRUE)[1:2], collapse = " ")
              })),
            by = "parsed", suffix = c("_wf1", "_wf2")) %>%
  filter(matched_wf1 != matched_wf2)

gbif <- read_csv("~/Documents/bogota_gbif.csv") %>%
  mutate(species_level = modify(gbif, function(x) {
      len <- str_split(x, " ", simplify = TRUE) %>% length()
      if (len == 1)
        FALSE 
      else 
        TRUE
    }) %>% as.logical()) %>%
  filter(species_level) %>%
  select(-species_level)


wf2 %>%
  mutate(matched = modify(matched, function(x) {
      paste(str_split(x, " ", simplify = TRUE)[1:2], collapse = " ")
    })) %>%
  left_join(gbif) %>%
  left_join(biotime %>% select(parsed, common)) %>%
  pull(gbif) %>% table() %>% table()
  print(n = 20000)

