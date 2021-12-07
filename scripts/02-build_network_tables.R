# Script to generate nodes and edges data.frame for use in shiny app
# Comprise links between packages, between databases, and between both
# Needed packages --------------------------------------------------------------
library("dplyr")


# Load initial data ------------------------------------------------------------
pkg_deps_df = readRDS("data/data_cleaned/pkg_deps_df.Rds")
included_pkg = readRDS("data/data_cleaned/included_pkg.Rds")

database_df = readxl::read_xlsx(
  "data/data_raw/Table comparing taxonomic tools.xlsx",
  sheet = 4, na = c("", "NA")
)

raw_db_links_df = readxl::read_xlsx(
  "data/data_raw/Table comparing taxonomic tools.xlsx",
  sheet = 6, na = c("", "NA")
)


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
      select(network_name, `Should we include this package in our review?`) %>%
      rename(
        pkg = network_name,
        inclusion = `Should we include this package in our review?`
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

saveRDS(taxonomy_dependencies_edge_df,
        "data/data_cleaned/taxonomy_pkg_dependencies.Rds")

# Database network -------------------------------------------------------------
# Make an attribute df with databases
access_df = included_pkg %>%
  select(`Package Name`, `Which authority?`) %>%
  mutate(db_list = stringr::str_split(`Which authority?`, ",")) %>%
  mutate(db_list = purrr::map(db_list, stringr::str_trim, side = "both")) %>%
  select(-`Which authority?`) %>%
  mutate(type = "accesses") %>%
  tidyr::unnest(c(db_list))

# data.frame of links between databases
db_links = raw_db_links_df %>%
  select(source_db = `Source database (= from)`,
         target_db = `Target database (= to)`) %>%
  mutate(link_type = "populates")

# Make a list of all databases (linked in network or accessed by package)
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

# Database graph
db_graph = igraph::graph_from_data_frame(db_links, vertices = all_db)

saveRDS(db_graph, "data/data_cleaned/db_igraph.Rds", compress = TRUE)

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
    select(1) %>%
    rename(id = `Package Name`) %>%
    mutate(node_type = "package",
           id = case_when(
             id == "TNRS" ~ "TNRS_pkg",
             id == "WorldFlora" ~ "WorldFlora_pkg",
             TRUE ~ id
           )),
  # Database list
  all_db %>%
    rename(id = db_list) %>%
    mutate(node_type = "db")
) %>%
  distinct() %>%
  add_row(id = "joelnitta/taxastand",   node_type = "package") %>%
  add_row(id = "EDIorg/taxonomyCleanr", node_type = "package") %>%
  add_row(id = "alexpiper/taxreturn",   node_type = "package") %>%
  add_row(id = "ropensci/taxview",      node_type = "package") %>%
  mutate(`Package Name` = id,
         label = id,
         `Object type` = node_type) %>%
  rename(group = node_type) %>%
  mutate(
  # Remove trailing user name when specifying node labels
    label = gsub(".*/", "", label),
    # Rename node type for ease of use in shiny app
    group = ifelse(group == "db", "database", group),
    # Make package labels bold
    label = ifelse(group == "package", paste0("<b>", label, "</b>"), label),
    title = ifelse(group == "database", paste0("<b>", label, "</b>"), label)) %>%
  filter(!(id %in% c("taxastand", "taxonomyCleanr", "taxreturn", "taxview")))


# Node Descriptions ------------------------------------------------------------

# Package Description
pkg_description = included_pkg %>%
  mutate(`Package Name` = case_when(
    `Package Name` == "TNRS"           ~ "TNRS_pkg",
    `Package Name` == "WorldFlora"     ~ "WorldFlora_pkg",
    `Package Name` == "taxastand"      ~ "joelnitta/taxastand",
    `Package Name` == "taxonomyCleanr" ~ "EDIorg/taxonomyCleanr",
    `Package Name` == "taxreturn"      ~ "alexpiper/taxreturn",
    `Package Name` == "taxview"        ~ "ropensci/taxview",
    TRUE ~ `Package Name`
  )) %>%
  semi_join(all_nodes %>%
              filter(group == "package"),
            by = c("Package Name" = "id")) %>%
  select(
    pkg_name    = `Package Name`,
    actively    = 
      `Actively Maintained (most recent activity on development version < 1 year)`,
    release_url = `Release URL (CRAN / Bioconductor)`,
    dev_url     = `Development Version`,
    step        = `At which step can it be used`,
    tax_group   = `If taxon-specific which taxon/taxa?`
  ) %>%
  mutate(
    html_info = paste0(
      "<b>Node Name</b>: <tt>", pkg_name, "</tt><br />",
      "<b>Type</b>: package<br />",
      "<b>Actively Maintained</b>: ", actively, "<br />",
      "<b>Workflow Step(s)</b>:<br />", step, "<br />",
      ifelse(
        !is.na(release_url),
        paste0(
          "<b>Release URL</b>:<br />",
          "<a href='", release_url, "'>", release_url,
          "</a><br />"
        ),
        ""
      ),
      ifelse(
        !is.na(dev_url),
        paste0(
          "<b>Development URL</b>:<br />",
          "<a href='", dev_url, "'>", dev_url,
          "</a><br />"
        ),
        ""
      )
    ),
    tax_group = ifelse(is.na(tax_group), "No taxonomic restriction", tax_group)
  )

# Create HTML info tag for databases
db_description = all_nodes %>%
  filter(group == "database") %>%
  select(id) %>%
  full_join(database_df, by = c(id = "Abbreviated name")) %>%
  mutate(
    html_info = paste0(
      "<b>Name</b>: ", id, "<br />",
      "<b>Full Name</b>: ", `Name in full`, "<br />",
      "<b>Type</b>: database<br />",
      "<b>Taxonomic Group</b>: ", `Taxonomic group`, "<br />",
      "<b>Spatial Scale</b>: ", `Spatial Scale`, "<br />",
      "<b>Taxonomic Breadth</b>: ", `Taxonomic Breadth`, "<br />",
      "<b>URL</b>: <a href='", URL, "'>", URL, "</a><br />"
    ),
    tax_group = case_when(
      `Taxonomic group` == "All life" ~ "No taxonomic restriction",
      `Taxonomic group` == "All life but Bacteria and Archea" ~
        "No taxonomic restriction",
      `Taxonomic group` == "All life but mainly animals, plants, and fungi" ~
        "No taxonomic restriction",
      TRUE ~ `Taxonomic group`
    )
  )
  

# Add Node Descriptions
all_nodes = all_nodes %>%
  full_join(
    bind_rows(
      pkg_description %>%
        select(id = pkg_name, html_info, tax_group),
      db_description %>%
        select(id, html_info, tax_group)
    ),
    by = "id"
  ) %>%
  rename(`Node Name`  = `Package Name`,
         `Tax. Group` = tax_group) %>%
  mutate(`Node Name` = case_when(
    `Node Name` ==  "TNRS_pkg"             ~ "TNRS",
    `Node Name` ==  "WorldFlora_pkg"       ~ "WorldFlora",
    `Node Name` ==  "joelnitta/taxastand"  ~ "taxastand",
    `Node Name` == "EDIorg/taxonomyCleanr" ~ "taxonomyCleanr",
    `Node Name` ==  "alexpiper/taxreturn"  ~ "taxreturn",
    `Node Name` ==  "ropensci/taxview"     ~ "taxview",
    TRUE ~ `Node Name`))

# Saving object ----------------------------------------------------------------

save(all_nodes, all_edges,
     file = "taxharmonizexplorer/shiny_data/full_network.Rdata")
