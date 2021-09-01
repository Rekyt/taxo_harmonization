message(" --- BOGOTA")

d <- read.csv("../data/data_cleaned/biotime_results/biotime_common.csv") %>%
  as_tibble() %>% 
  select(-class, -phylum, -common) #not needed in this pipeline
message("     LCVP")

# plants  ---------
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
  write_csv("../data/data_cleaned/biotime_results/bogota_lcvp.csv")

# fishses ---------
message("     FishBase")
fishbase <- parSapply(
  cl,
  d$parsed,
  function(x) rfishbase::validate_names(x)[1]
)
fishbase <- tibble(parsed = d$parsed, fishbase = fishbase)
write_csv(fishbase, "../data/data_cleaned/biotime_results/bogota_fishbase.csv")

# birds ---------
message("     ebird")
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
write_csv(birds, "../data/data_cleaned/biotime_results/bogota_ebird.csv")
message("      Bogota running time:", Sys.time() - T0)
