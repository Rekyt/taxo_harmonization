# Companion script to taxonomic harmonization project

# Load packages ----------------------------------------------------------------
library("dplyr")
library("ggraph")

# Load and wrangle data --------------------------------------------------------

# Raw data from the Google Doc sheets exported as XLSX file
pkg_df = readxl::read_xlsx("data/Table comparing taxonomic tools.xlsx",
                           na = c("", "NA"))

# Get the list of packages that are included in the review
inc_pkg = pkg_df %>%
  filter(`Should we include this package in our review?` == "include")

# List of included packages that are on CRAN
cran_pkg = inc_pkg %>%
  filter(!is.na(`Release URL (CRAN / Bioconductor)`))


# Automatic package dependency list through {crandep} --------------------------
# Get raw data

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


# Dependency network with {cranly} ---------------------------------------------

p_db            = tools::CRAN_package_db()
package_db      = cranly::clean_CRAN_db(p_db)
package_network = cranly::build_network(package_db)

taxo_pkgs = cranly::package_with(package_network, exact = TRUE,
                                 name = cran_pkg$`Package Name`)

plot(package_network, package = taxo_pkgs)

taxo_pkgs_only <- subset(package_network, package = taxo_pkgs, only = TRUE)
plot(taxo_pkgs_only, package = taxo_pkgs)


# Dependency network with {pkgdepends} -----------------------------------------

# Retrieve dependencies for all included packages
all_pkgs = inc_pkg %>%
  mutate(sub_name = case_when(
    `Package Name` == "traitdataform" ~ "EcologicalTraitData/traitdataform",
    `Package Name` == "taxizedb" ~ "ropensci/taxizedb",
    !is.na(`Release URL (CRAN / Bioconductor)`) ~ `Package Name`,
    !is.na(`Development Version`) ~ gsub("https://github.com/", "",
                                         `Development Version`),
    TRUE ~ NA_character_
  )) %>%
  add_row(sub_name = "joelnitta/jntools") %>%
  pull(sub_name) %>%
 pkgdepends::new_pkg_deps()

all_pkgs$resolve()
all_pkgs$solve()
all_pkgs$draw()

all_pkgs_df = all_pkgs$get_resolution()


# Create actual igraph network -------------------------------------------------

# Edge list data.frame
dep_df = all_pkgs_df %>%
  select(pkg = ref, deps) %>%
  tidyr::unnest(deps) %>%
  select(pkg, package, type) %>%
  mutate(type = tolower(type)) %>%
  distinct()

# Vertex attribute data frame
pkg_info_df = bind_rows(
  distinct(dep_df, pkg),
  distinct(dep_df, pkg = package)
  ) %>%
  distinct() %>%
  full_join(
    pkg_df %>%
      mutate(sub_name = case_when(
        `Package Name` == "traitdataform" ~ "EcologicalTraitData/traitdataform",
        `Package Name` == "taxizedb" ~ "ropensci/taxizedb",
        !is.na(`Release URL (CRAN / Bioconductor)`) ~ `Package Name`,
        !is.na(`Development Version`) ~ gsub("https://github.com/", "",
                                             `Development Version`))) %>%
      select(sub_name, 3:4) %>%
      rename(
        pkg = sub_name,
        inclusion = `Should we include this package in our review?`,
        category  = `Is this package central in taxonomic harmonization workflow?`
      ),
    by = c("pkg")
  )

# Actual graph object
dep_graph = dep_df %>%
  filter(type != "enhances", type != "linkingto") %>%
  igraph::graph_from_data_frame(vertices = pkg_info_df)

# Make smaller graph with only taxonomic packages that directly depends
# from each other
taxo_df = dep_df %>%
  filter(pkg %in% inc_pkg$`Package Name` & package %in% inc_pkg$`Package Name`)

# Make an attribute df with database
access_df = inc_pkg %>%
  select(`Package Name`, `Which authority?`) %>%
  mutate(db_list = stringr::str_split(`Which authority?`, ",")) %>%
  mutate(db_list = purrr::map(db_list, stringr::str_trim, side = "both")) %>%
  select(-`Which authority?`) %>%
  mutate(type = "accesses") %>%
  tidyr::unnest(c(db_list))

db_links = data.frame(
  final_db = c("GBIF"),
  source_db = c("COL"),
  type = "includes"
)

# Network Visualization --------------------------------------------------------
tg_dep = dep_graph %>%
  tidygraph::as_tbl_graph()
  
tg_dep %>%
  ggraph(layout = "kk") +
  geom_edge_link() + 
  geom_node_point(aes(colour = category))
