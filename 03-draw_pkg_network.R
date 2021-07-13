# Script to draw network of packages
# Packages ---------------------------------------------------------------------
library("ggraph")
library("tidygraph")
library("dplyr")


# Load data --------------------------------------------------------------------

# Node List
included_pkg = readRDS("data_cleaned/included_pkg.Rds") %>%
  distinct(network_name) %>%
  mutate(network_name = gsub(".*/", "", network_name))

# Edge List
tax_edges = readRDS("data_cleaned/taxonomy_pkg_dependencies.Rds") %>%
  mutate(source_pkg = gsub(".*/", "", source_pkg),
         dependency = gsub(".*/", "", dependency))

# Create network
tax_pkg_graph = tidygraph::tbl_graph(included_pkg, tax_edges) %>%
  mutate(degree = tidygraph::centrality_degree())


# Actual Network ---------------------------------------------------------------
pkg_network = ggraph(tax_pkg_graph, layout = "fr") + 
  geom_edge_link(edge_width = 1, colour = "#88b4da",
                 arrow = arrow(length = unit(3, 'mm'), angle = 15),) + 
  geom_node_point(size = 3, shape = 21, colour = "white", fill = "black") +
  geom_node_text(aes(label = network_name), family = "Consolas",
                 fontface = "bold", vjust = 2, size = 4) +
  theme_void()

# Save Network -----------------------------------------------------------------
ggsave("figures/network_of_packages.png", pkg_network)
