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


# Package igraph network -------------------------------------------------------

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

# Viz. Package Network ---------------------------------------------------------
tg_dep = dep_graph %>%
  tidygraph::as_tbl_graph()

tg_dep %>%
  ggraph(layout = "kk") +
  geom_edge_link() + 
  geom_node_point(aes(colour = category))

taxo_graph = igraph::graph_from_data_frame(
  taxo_df[, c(2, 1, 3)], vertices = inc_pkg)

taxo_graph %>%
  ggraph(layout = "igraph", algorithm = "nicely") +
  geom_edge_link(
    arrow = arrow(type = "closed", length = unit(4, "mm"), angle = 7),
    alpha = 1/2
  ) +
  geom_node_point(
    aes(
      fill = `Is this package central in taxonomic harmonization workflow?`
    ),
      shape = 21, color = "white", size = 3) +
  geom_node_label(aes(label = name), family = "Consolas", repel = TRUE) +
  theme_void() +
  theme(legend.position = "top")
 
# Database network -------------------------------------------------------------
# Make an attribute df with database
access_df = inc_pkg %>%
  select(`Package Name`, `Which authority?`) %>%
  mutate(db_list = stringr::str_split(`Which authority?`, ",")) %>%
  mutate(db_list = purrr::map(db_list, stringr::str_trim, side = "both")) %>%
  select(-`Which authority?`) %>%
  mutate(type = "accesses") %>%
  tidyr::unnest(c(db_list)) %>%
  mutate(db_list = case_when(
    db_list == "FishBase (Eschmeyer's Catalog of Fishes)" ~ "FishBase",
    db_list == "WikiData" ~ "Wikidata",
    db_list == "INPI"     ~ "IPNI",
    db_list == "World Flora Online" ~ "WorldFlora",
    db_list == "vegetplant" ~ "GermanSL",
    db_list == "Plants of the World" ~ "POWO",
    db_list == "multiple" ~ "FinBIF",
    db_list == "Tropics" ~ "Tropicos",
    TRUE ~ db_list
  ))

db_links = tibble::tribble(
  ~source_db, ~target_db, ~link_type,
  "COL",            "GBIF",        "populates",
  "COL",            "SeaLifeBase", "populates",
  "COL",            "EOL",         "populates",
  "COL",            "GNR",         "populates",
  "Index Fungorum", "Wikidata",    "populates",
  "Index Fungorum", "COL",         "populates",
  "FishBase",       "COL",         "populates",
  "FishBase",       "GNR",         "populates",
  "WoRMS",          "COL",         "populates",
  "WoRMS",          "Wikidata",    "populates",
  "Wikispecies",    "Wikipedia",   "populates",
  "Wikispecies",    "Wikidata",    "populates",
  "Wikispecies",    "GNR",         "populates",
  "Wikipedia",      "GNR",         "populates",
  "Wikidata",       "GNR",         "populates",
  "TPL",            "WorldFlora",  "populates",
  "WorldFlora",     "TNRS",        "populates",
  "eBird/Clements", "GNR",         "populates",
  "BirdLife",       "GNR",         "populates",
  "ZooBank",        "GNR",         "populates",
  "POWO",           "WCPS",        "populates",
  "POWO",           "IPNI",        "populates",
  "WCPS",           "COL",         "populates",
  "IPNI",           "GNR",         "populates",
  "AlgaeBase",      "SeaLifeBase", "populates",
  "ITIS",           "GNR",         "populates",
  "ITIS",           "EOL",         "populates",
  "ITIS",           "Tropicos",    "populates",
  "ITIS",           "NatureServe", "populates",
  "ITIS",           "COL",         "populates",
  "Tropicos",       "USDA",        "populates",
  "Tropicos",       "TNRS",        "populates",
  "Tropicos",       "GNR",         "populates",
  "USDA",           "TNRS",        "populates",
  "NCBI",           "GNR",         "populates",
  "GBIF",           "GNR",         "populates",
  "EOL",            "GNR",         "populates"
)

all_db = access_df %>%
  distinct(db_list) %>%
  bind_rows(
    db_links %>%
      distinct(db_list = source_db),
    db_links %>%
      distinct(db_list = target_db)
  ) %>%
  distinct() %>%
  filter(!is.na(db_list))

db_graph = igraph::graph_from_data_frame(db_links, vertices = all_db)

# Viz. DB network --------------------------------------------------------------
db_graph %>%
  tidygraph::as_tbl_graph() %>%
  ggraph(layout = "igraph", algorithm = "nicely") +
  geom_edge_link(arrow = arrow(type = "closed",
                               length = unit(4, "mm"), angle = 7), alpha = 1/2) +
  geom_node_point(shape = 21, color = "white", fill  = "black") +
  geom_node_text(aes(label = name), check_overlap = TRUE, repel = TRUE) +
  theme_void()
  

# Joining both networks --------------------------------------------------------
