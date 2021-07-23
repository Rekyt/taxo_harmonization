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

biotime <- read_csv("data_raw/biotime.txt", col_names = "BioTIME") %>%
  as_tibble() %>% 
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

cl <- makeCluster(4) #parallize operation thought the whole script

# WF1: Torino ----------
source("torino.R")

# WF2: Bogota -------
source("bogota.R") #conflicts are resolved in combine-results-workflows.R

# WF3: GBIF only with pre-processing -----------
d <- read_csv("biotime_results/biotime_common.csv") %>%
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
   write_csv("biotime_results/gbif_preproc.csv")

# WF4: GBIF only --------------
gbif <- parSapply(
  cl,
  biotime$BioTIME,
  function(x) {
    ans <- rgbif::name_backbone(x, strict = FALSE) #no fuzzy
    if ("canonicalName" %in% names(ans))
      return(ans$canonicalName)
    else
      return(NA)
  }
)
tibble(biotime = names(gbif), gbif = as.vector(gbif)) %>%
  write_csv("biotime_results/gbif_only.csv")

stopCluster(cl)
