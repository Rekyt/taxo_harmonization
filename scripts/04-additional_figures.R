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
  scale_size(range = c(1.5, 7)) +
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
    mutate(
      network_name = ifelse(
        network_name == "ropensci/taxizedb", "taxizedb", network_name
      )
    )
)

access_tpl = included_pkg %>%
  select(network_name, `Which authority?`) %>%
  mutate(
    network_name = ifelse(
    network_name == "ropensci/taxizedb", "taxizedb", network_name
  ),
  access_tpl = grepl("TPL", `Which authority?`, fixed = TRUE)
  ) %>%
  filter(access_tpl) %>%
  pull(network_name)

is_ropensci = included_pkg %>%
  select(dev_version = `Development Version`, network_name) %>%
  mutate(
    network_name = ifelse(
      network_name == "ropensci/taxizedb", "taxizedb", network_name
    ),
    is_ropensci = grepl("ropensci", dev_version, fixed = TRUE)
  ) %>%
  filter(is_ropensci) %>%
  pull(network_name)

plot_pkg_network = ggraph(pkg_network, layout = "nicely") + 
  geom_edge_link(
    edge_width = 0.7, colour = "#BBCCD0",
    arrow = arrow(length = unit(3, 'mm'), angle = 15, type = "closed")
  ) + 
  geom_node_point(aes(fill = name %in% access_tpl,
                      colour = name %in% is_ropensci),
                  size = 3.2, shape = 21, stroke = 1) +
  geom_node_text(
    aes(label = gsub("[[:alpha:],-]+/", "", name)), family = "mono",
    fontface = "bold", vjust = 2, size = 4, check_overlap = TRUE
  ) +
  scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = NULL)) +
  labs(fill = "Access The Plant List?",
       colour = "Is rOpenSci package?") +
  theme_void() +
  theme(legend.position = "top")

plot_pkg_network

ggsave("figures/figX_packages_network.svg", plot_pkg_network, width = 700,
       height = 400, units = "px", dpi = 300, scale = 4)

ggsave("figures/figX_packages_network.png", plot_pkg_network, width = 700,
       height = 400, units = "px", dpi = 300, scale = 4)
