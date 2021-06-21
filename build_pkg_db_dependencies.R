# Script to generate list of dependencies of packages
# Load packages ----------------------------------------------------------------
library("dplyr")

# Load and wrangle data --------------------------------------------------------

# Raw data from the Google Doc sheets exported as XLSX file
raw_pkg_table = readxl::read_xlsx("data_raw/Table comparing taxonomic tools.xlsx",
                                  na = c("", "NA"))

# Get the list of packages that are included in the review
included_pkg = raw_pkg_table %>%
  filter(`Should we include this package in our review?` == "include")

# List of included packages that are on CRAN
cran_pkg = included_pkg %>%
  filter(!is.na(`Release URL (CRAN / Bioconductor)`))


# Dependency network with {pkgdepends} -----------------------------------------

# Create column 'network_name' to extract deps with pkgdepends
included_pkg = included_pkg %>%
  mutate(network_name = case_when(
    # Change taxizedb to be compatible with ropensci/taxview dependency
    `Package Name` == "taxizedb" ~ "ropensci/taxizedb",
    # If package on CRAN directly use its name
    !is.na(`Release URL (CRAN / Bioconductor)`) ~ `Package Name`,
    # Otherwise get GitHub andle
    !is.na(`Development Version`) ~ gsub("https://github.com/", "",
                                         `Development Version`),
    TRUE ~ NA_character_
  )) %>%
  # Additional dependencies not mentioned explicitly in packages
  add_row(network_name = "joelnitta/jntools") %>%
  add_row(network_name = "gustavobio/tpldata")

# Retrieve dependencies for all included packages
pkg_deps = included_pkg %>%
  pull(network_name) %>%
  pkgdepends::new_pkg_deps()

pkg_deps$resolve()
pkg_deps$solve()
pkg_deps$draw()

pkg_deps_df = pkg_deps$get_resolution()

saveRDS(pkg_deps_df, "data_cleaned/pkg_deps_df.Rds", compress = TRUE)
saveRDS(included_pkg, "data_cleaned/included_pkg.Rds")
