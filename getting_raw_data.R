# Companion script to taxonomic harmonization project

# Load packages ----------------------------------------------------------------
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

# Need to interface with {gh} to search through GitHub automatically
# Find a package to get data from Bioconductor too


# Visualize package dependency network -----------------------------------------
# Get raw data
pkg_df = readxl::read_xlsx("data/Table comparing taxonomic tools.xlsx",
                           na = c("", "NA"))
cran_pkg = pkg_df %>%
  filter(`Should we include this package in our review?` == "include",
         !is.na(`Release URL (CRAN / Bioconductor)`))

# Get package dependency
deps_pkgs = cran_pkg %>%
  filter(grepl("cran", `Release URL (CRAN / Bioconductor)`)) %>%
  select(pkg_name = `Package Name`) %>%
  mutate(dep_df = purrr::map(pkg_name,
                             ~.x %>%
                               crandep::get_dep(c("Imports")) %>%
                               tibble::enframe("dep_type", "dep_name") %>%
                               mutate(dep_type = "Imports"))) %>%
  tidyr::unnest(dep_df)

# Alternative to build dependency network --------------------------------------

p_db            = tools::CRAN_package_db()
package_db      = cranly::clean_CRAN_db(p_db)
package_network = cranly::build_network(package_db)

taxo_pkgs = cranly::package_with(package_network, exact = TRUE,
                                 name = cran_pkg$`Package Name`)

plot(package_network, package = taxo_pkgs)

taxo_pkgs_only <- subset(package_network, package = taxo_pkgs, only = TRUE)
plot(taxo_pkgs_only, package = taxo_pkgs)

# Try searching GitHub packages ------------------------------------------------


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
is_pkg = distinct_pkgs %>%
  pull(repo_name) %>%
  lapply(function(repo_name) {
    
    Sys.sleep(5)
    
    has_description = gh::gh(
      "GET /search/code", q = paste0("filename:DESCRIPTION+repo:", repo_name)
    )
    
    length(has_description$items) > 0
  })
