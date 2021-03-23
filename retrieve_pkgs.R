# This script aims to retrieve a list of potential interesting packages from
# various sources by matching names
# Packages ---------------------------------------------------------------------
library("dplyr")


# Automatic retrieval of packages ----------------------------------------------
# Currently only retrieve packages from CRAN
current_date = format.Date(Sys.Date())

search_terms = c("taxonomy", "taxon", "taxa", "taxonomic", "taxonomical")

cran_pkgs = search_terms %>%
  lapply(pkgsearch::ps) %>%
  bind_rows() %>%
  distinct() %>%
  mutate(query_date = current_date)


# Retrieve GitHub packages with GH API -----------------------------------------

gh_query = gh::gh("GET /search/repositories", q = "taxonomy+language:R")

# Format query
gh_df = lapply(search_terms, function(term) {
  
  # Query R Repo that have the term in either name/description or in README
  gh_query = gh::gh("GET /search/repositories", q = paste0(term, "+language:R"),
                    per_page = 100)
  
  add_items= list()
  
  if (gh_query$total_count > 100) {
    n_pages = 1 + gh_query$total_count %/% 100
    
    add_items = lapply(
      2:n_pages,
      function(n_page) {
        gh::gh("GET /search/repositories", q = paste0(term, "+language:R"),
               per_page = 100, page = n_page)$items
      }
    )
  }
  
  all_items = purrr::flatten(c(gh_query["items"], add_items))
  
  lapply(all_items, function(x) {
    
    data.frame(search_term = term, name = x$name, repo_name = x$full_name,
               description = x$description)
  }) %>%
    bind_rows()
}) %>%
  bind_rows() %>%
  distinct()

distinct_pkgs = gh_df %>%
  select(-search_term) %>%
  distinct() %>%
  as_tibble()

# Next step is repo probably a package (does it has a DESCRIPTION file?)
is_pkg_v = distinct_pkgs %>%
  pull(repo_name) %>%
  lapply(function(repo_name) {
    
    Sys.sleep(2)
    
    has_description = gh::gh(
      "GET /search/code", q = paste0("filename:DESCRIPTION+repo:", repo_name)
    )
    
    length(has_description$items) > 0
  }) %>%
  unlist()

distinct_pkgs = distinct_pkgs %>%
  mutate(is_pkg = is_pkg_vec)

# Querying rdrr.io -------------------------------------------------------------
# Try querying the undocumneted rdrr.io API to get a list of packages
# https://rdrr.io/api/find/?repos=bioc%2Crforge%2Cgithub&page=0&fuzzy_slug=taxa
base_url = "https://rdrr.io/api/find/"

ko = httr::GET(base_url, query = list(repos = "bioc,rforge,github", page = 0,
                                      fuzzy_slug = "taxa"))
