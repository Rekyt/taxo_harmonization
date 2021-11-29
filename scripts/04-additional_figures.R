# Script to plot additional figures related to presentation of our work
# Packages ---------------------------------------------------------------------
library("dplyr")
library("ggplot2")
library("ggraph")

# Load data --------------------------------------------------------------------

# Databases data
database_df = readxl::read_xlsx(
  "data/data_raw/Table comparing taxonomic tools.xlsx",
  sheet = 4, na = c("", "NA")
)

raw_db_links_df = readxl::read_xlsx(
  "data/data_raw/Table comparing taxonomic tools.xlsx",
  sheet = 6, na = c("", "NA")
)

# Packages data
pkg_deps_df = readRDS("data/data_cleaned/pkg_deps_df.Rds")
pkg_tax_deps = readRDS("data/data_cleaned/taxonomy_pkg_dependencies.Rds")
included_pkg = readRDS("data/data_cleaned/included_pkg.Rds")

# Figure of Number of Databases Indexed by categories --------------------------

fig_number_dbs = database_df %>%
  mutate(
    `Spatial Scale` = `Spatial Scale` %>%
           factor(levels = c("Regional", "Global")),
    `Taxonomic Breadth` = `Taxonomic Breadth` %>%
      factor(levels = c("Small", "Medium", "Large"))
    ) %>% 
  ggplot(aes(`Taxonomic Breadth`, `Spatial Scale`)) +
  geom_count(aes(color = after_stat(n), size = after_stat(n))) +
  guides(color = 'legend') +
  scale_color_viridis_c() +
  labs(color = "N. databases", size = "N. databases") +
  theme_bw(18) +
  theme(legend.position = "top")


# Relationship network between databases ---------------------------------------

# see '03-draw_db_network.R'

# Network of packages ----------------------------------------------------------

pkg_network = igraph::graph_from_data_frame(
  pkg_tax_deps %>%
    mutate(source_pkg = ifelse(source_pkg == "ropensci/taxizedb", "taxizedb",
                               source_pkg)),
  vertices = included_pkg %>%
    select(network_name) %>%
    mutate(network_name = ifelse(
      network_name == "ropensci/taxizedb", "taxizedb", network_name
      )
    )
)

plot_pkg_network = ggraph(pkg_network, layout = "kk") + 
  geom_edge_link(
    edge_width = 1, colour = "#88b4da",
    arrow = arrow(length = unit(5, 'mm'), angle = 4, type = "closed")
  ) + 
  geom_node_point(size = 3, shape = 21, colour = "white", fill = "black") +
  geom_node_text(
    aes(label = gsub("[[:alpha:],-]+/", "", name)), family = "mono",
    fontface = "bold", vjust = 2, size = 4, check_overlap = TRUE
  ) +
  theme_void()

plot_pkg_network

