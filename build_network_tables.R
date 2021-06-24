# Script to generate nodes and edges data.frame for use in shiny app
# Comprise links between packages, between databases, and between both
# Needed packages --------------------------------------------------------------
library("dplyr")

# Load initial data ------------------------------------------------------------
pkg_deps_df <- readRDS("data_cleaned/pkg_deps_df.Rds")
included_pkg <- readRDS("data_cleaned/included_pkg.Rds")

# Extract network in two data.frames -------------------------------------------
# Edge list data.frame
dependencies_edge_df = pkg_deps_df %>%
  select(pkg = ref, deps) %>%
  tidyr::unnest(deps) %>%
  select(source_pkg = pkg, dependency = package, dependency_type = type) %>%
  mutate(dependency_type = tolower(dependency_type)) %>%
  distinct()

# Vertex attribute data frame
pkg_info_df = bind_rows(
  distinct(dependencies_edge_df, pkg = source_pkg),
  distinct(dependencies_edge_df, pkg = dependency)
) %>%
  distinct() %>%
  full_join(
    included_pkg %>%
      select(network_name, 4:5) %>%
      rename(
        pkg = network_name,
        inclusion = `Should we include this package in our review?`,
        category  = `Is this package central in taxonomic harmonization workflow?`
      ),
    by = c("pkg")
  )

# Make smaller graph with only taxonomic packages that directly depends
# from each other
taxonomy_dependencies_edge_df = dependencies_edge_df %>%
  filter(
    # Package name is within curate taxonomy package list
    source_pkg %in% c(included_pkg$network_name, included_pkg$`Package Name`) &
      # Dependency also! (no need to look at network_name as dependencies
      # are written only with package name)
      dependency %in% included_pkg$`Package Name`)

# Database network -------------------------------------------------------------
# Make an attribute df with databases
access_df = included_pkg %>%
  select(`Package Name`, `Which authority?`) %>%
  mutate(db_list = stringr::str_split(`Which authority?`, ",")) %>%
  mutate(db_list = purrr::map(db_list, stringr::str_trim, side = "both")) %>%
  select(-`Which authority?`) %>%
  mutate(type = "accesses") %>%
  tidyr::unnest(c(db_list))

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
  "eBird",           "GNR",         "populates",
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

saveRDS(db_graph, "data_cleaned/db_igraph.Rds", compress = TRUE)

# Viz. DB network --------------------------------------------------------------
if (FALSE) {
  db_graph %>%
    tidygraph::as_tbl_graph() %>%
    ggraph(layout = "igraph", algorithm = "nicely") +
    geom_edge_link(arrow = arrow(type = "closed",
                                 length = unit(4, "mm"), angle = 7), alpha = 1/2) +
    geom_node_point(shape = 21, color = "white", fill  = "black") +
    geom_node_text(aes(label = name), check_overlap = TRUE, repel = TRUE) +
    theme_void()
}

# Joining both networks --------------------------------------------------------

# Combine all edges
all_edges = bind_rows(
  # Links between pkgs
  taxonomy_dependencies_edge_df %>%
    filter(dependency_type %in% c("imports", "depends")) %>%
    select(-dependency_type) %>%
    distinct() %>%
    rename(from = source_pkg, to = dependency) %>%
    mutate(type = "depends"),
  # Links between pkg and dbs
  access_df %>%
    rename(from = `Package Name`, to = db_list) %>%
    filter(!is.na(to)) %>%
    mutate(from = case_when(
      from == "TNRS" ~ "TNRS_pkg",
      from == "WorldFlora" ~ "WorldFlora_pkg",
      TRUE ~ from
    )),
  # Links between DBs
  db_links %>%
    rename(from = source_db, to = target_db, type = link_type)
) %>%
  # Define some columns for use in visNetwork in shiny app
  mutate(arrows = case_when(type == "depends" ~ "to",
                            type == "accesses" ~ "to",
                            type == "populates" ~ "to"),
         color = case_when(type == "depends" ~ "#998EC3",
                           type == "accesses" ~ "steelblue",
                           type == "populates" ~ "#B35806")) 

# Combine all nodes
all_nodes = bind_rows(
  # Packages list with metadata
  included_pkg %>%
    # Remove remaining packages used for dependency detection in table
    filter(
      !(network_name %in% c("joelnitta/jntools", "gustavobio/tpldata"))
    ) %>%
    select(1, 5) %>%
    rename(
      id = `Package Name`,
      workflow_importance =
        `Is this package central in taxonomic harmonization workflow?`
    ) %>%
    mutate(node_type = "package",
           id = case_when(
             id == "TNRS" ~ "TNRS_pkg",
             id == "WorldFlora" ~ "WorldFlora_pkg",
             TRUE ~ id
           )),
  # Database list
  all_db %>%
    rename(id = db_list) %>%
    mutate(workflow_importance = "other", node_type = "db")
) %>%
  distinct() %>%
  add_row(id = "joelnitta/taxastand", workflow_importance = "secondary",
          node_type = "package") %>%
  add_row(id = "EDIorg/taxonomyCleanr", workflow_importance = "secondary",
          node_type = "package") %>%
  add_row(id = "alexpiper/taxreturn", workflow_importance = "secondary",
          node_type = "package") %>%
  add_row(id = "ropensci/taxview", workflow_importance = "secondary",
          node_type = "package")

all_nodes$title <- paste0("<p><b>", all_nodes$id,"</b><br>Some website</p>")
all_nodes$`Package name` <- all_nodes$label <- all_nodes$id
all_nodes$`Object type` <- all_nodes$node_type

all_nodes <- rename(all_nodes, group = node_type) %>%
  mutate(
  # Remove trailing user name when specifying node labels
    label = gsub(".*/", "", label),
    # Rename node type for ease of use in shiny app
    group = ifelse(group == "db", "database", group),
    # Make package labels bold
    label = ifelse(group == "package", paste0("<b>", label, "</b>"), label))


# Node Description -------------------------------------------------------------

node_description = included_pkg %>%
  semi_join(all_nodes, by = c("Package Name" = "id")) %>%
  select(
    pkg_name    = `Package Name`,
    actively    = `Actively Maintained (most recent activity on development version < 1 year)`,
    release_url = `Release URL (CRAN / Bioconductor)`,
    dev_url     = `Development Version`,
    step        = `At which step can it be used`
  ) %>%
  mutate(
    html_info = paste0(
      "Node name: <tt>", pkg_name, "</tt><br />",
      "Type: package<br />",
      "Actively maintained: ", actively, "<br />",
      ifelse(
        !is.na(release_url),
        paste0(
          "Release URL: ",
          "<a href='", release_url, ">", release_url,
          "</a><br />"
        ),
        ""
      ),
      ifelse(
        !is.na(dev_url),
        paste0(
          "Development URL: ",
          "<a href='", dev_url, ">", dev_url,
          "</a><br />"
        ),
        ""
      )
    )
  )

all_nodes = all_nodes %>%
  full_join(node_description %>%
              select(pkg_name, html_info),
            by = c(id = "pkg_name"))

# Saving object ----------------------------------------------------------------

save(all_nodes, all_edges, file = "taxtool-selecter/shiny_data/full_network.Rdata")
