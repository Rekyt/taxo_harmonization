message(" --- TORINO ---")

# get taxonomic class --------
message("     GBIF taxonomic group")
classes <- parSapply(
  cl,
  biotime$parsed,
  function(x) rgbif::name_backbone(x, strict = FALSE)$class
)
classes <- tibble(parsed = names(unlist(classes)),
                  class = as.vector(unlist(classes))) %>%
  unnest(cols = class)
biotime %<>% left_join(classes %>% distinct_all())

# get phylum ------
phylum <- parSapply(
  cl,
  biotime %>% 
    pull(class) %>% 
    unique(),
  function(x) rgbif::name_backbone(x, strict = FALSE)$phylum #no fuzzy
)
phylum <- tibble(class = names(unlist(phylum)),
                 phylum = as.vector(unlist(phylum))) %>% 
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
write_csv(biotime, "data/data_cleaned/biotime_results/biotime_common.csv")

# harmonizing against databases
d <- suppressMessages(
  read_csv("data/data_cleaned/biotime_results/biotime_common.csv")
)

# vascular plants ------------
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
  write_csv("data/data_cleaned/biotime_results/torino_lcvp.csv")

# fishes -----------
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
write_csv(fishbase, "data/data_cleaned/biotime_results/torino_fishbase.csv")
message("     ", Sys.time() - T0)

# birds ---------------
message("     eBird")
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
write_csv(birds, "data/data_cleaned/biotime_results/torino_ebird.csv")
message("      Torino running time:", Sys.time() - T0)
