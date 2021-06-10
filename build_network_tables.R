# Package igraph network -------------------------------------------------------
pkg_deps_df <- readRDS("data_cleaned/pkg_deps_df.Rds")

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

if (FALSE) {
  # Actual graph object
  dependency_graph = dependencies_edge_df %>%
    filter(dependency_type != "enhances", dependency_type != "linkingto") %>%
    igraph::graph_from_data_frame(vertices = pkg_info_df)
}

# Make smaller graph with only taxonomic packages that directly depends
# from each other
taxonomy_dependencies_edge_df = dependencies_edge_df %>%
  filter(
    # Package name is within curate taxonomy package list
    source_pkg %in% c(included_pkg$network_name, included_pkg$`Package Name`) &
      # Dependency also! (no need to look at network_name as dependencies
      # are written only with package name)
      dependency %in% included_pkg$`Package Name`)

if (FALSE) {
  taxonomy_pkg_graph = igraph::graph_from_data_frame(
    taxonomy_dependencies_edge_df,
    vertices = pkg_info_df %>%
      filter(pkg %in% c(included_pkg$network_name, included_pkg$`Package Name`)))
  
  saveRDS(taxonomy_pkg_graph, "data_cleaned/taxonomy_pkg_graph.Rds",
          compress = TRUE)
  
  saveRDS(taxonomy_pkg_graph,
          "taxtool-selecter/shiny_data/taxonomy_pkg_graph.Rds")
}

# Viz. Package Network ---------------------------------------------------------
# Visualize the network
if (FALSE) {
  taxonomy_pkg_graph %>%
    ggraph(layout = "igraph", algorithm = "nicely") +
    geom_edge_link(
      arrow = arrow(type = "closed", length = unit(4, "mm"), angle = 7),
      alpha = 1/2
    ) +
    geom_node_point(
      aes(
        fill = category
      ),
      shape = 21, color = "white", size = 3) +
    geom_node_label(aes(label = name), family = "monospace", repel = TRUE) +
    theme_void() +
    theme(legend.position = "top")
}
# Database network -------------------------------------------------------------
# Make an attribute df with database
access_df = included_pkg %>%
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

if (FALSE) {
  db_graph = igraph::graph_from_data_frame(db_links, vertices = all_db)
  
  saveRDS(db_graph, "data_cleaned/db_igraph.Rds", compress = TRUE)
  saveRDS(db_graph, "taxtool-selecter/shiny_data/db_igraph.Rds")
}
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
)

all_nodes = bind_rows(
  # Packages list with metadata
  included_pkg %>%
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


all_nodes <- all_nodes %>% mutate(shape = case_when(
  node_type == "db" ~ "diamond",
  node_type == "package" ~ "dot",
  TRUE ~ node_type
)) %>% mutate(color = case_when(
  node_type == "db" ~ "blue",
  node_type == "package"~ "green",
  TRUE ~ node_type
))

all_nodes <- rename(all_nodes, group = node_type)

save(all_nodes, all_edges, file = "taxtool-selecter/shiny_data/full_network.Rdata")

if (FALSE) {
  all_graph = igraph::graph_from_data_frame(
    all_edges, vertices = all_nodes
  )
  
  saveRDS(all_graph, "taxtool-selecter/shiny_data/full_network.Rds")
  
  plot_full_network = all_graph %>%
    ggraph(layout = "igraph", algorithm = "nicely") +
    geom_edge_link(
      aes(color = type), alpha = 2/3,
      arrow = arrow(type = "closed", length = unit(2, "mm"), angle = 7),
      end_cap = circle(2, 'mm')
    ) +
    # Package & DBpoints
    geom_node_point(
      aes(shape = node_type), color = "white", fill = "black", size = 3,
    ) +
    # Labels
    geom_node_text(
      aes(label = name,
          family = ifelse(node_type == "package", "Consolas", "Helvetica")
      ), check_overlap = TRUE, repel = TRUE
    ) +
    scale_shape_manual(values = c(db = 22, package = 21),
                       labels = c(db = "Database", package = "Package"),
                       name = "bla") +
    scale_edge_color_brewer(type = "qual") +
    theme_void() +
    theme(legend.position = "top")
  
  plot_full_network
}

