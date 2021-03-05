library("dplyr")

current_date = format.Date(Sys.Date())

search_terms = c("taxonomy", "taxon", "taxa", "taxonomic", "taxonomical")

cran_pkgs = search_terms %>%
  lapply(pkgsearch::ps) %>%
  bind_rows() %>%
  distinct() %>%
  mutate(query_date = current_date)


pkg_df = readxl::read_xlsx("data/Table comparing taxonomic tools.xlsx",
                           na = c("", "NA"))
cran_pkg = pkg_df %>%
  filter(`Should we include this package in our review?` == "include",
         !is.na(`Release URL (CRAN / Bioconductor)`))
  
deps_pkgs = cran_pkg %>%
  filter(grepl("cran", `Release URL (CRAN / Bioconductor)`)) %>%
  select(pkg_name = `Package Name`) %>%
  mutate(dep_df = purrr::map(pkg_name,
                             ~.x %>%
                               crandep::get_dep(c("Imports")) %>%
                               tibble::enframe("dep_type", "dep_name") %>%
                               mutate(dep_type = "Imports")))
