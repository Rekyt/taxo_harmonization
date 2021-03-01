library("dplyr")

current_date = format.Date(Sys.Date())

search_terms = c("taxonomy", "taxon", "taxa", "taxonomic", "taxonomical")

cran_pkgs = search_terms %>%
  lapply(pkgsearch::ps) %>%
  bind_rows() %>%
  distinct() %>%
  mutate(query_date = current_date)
