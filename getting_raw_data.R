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