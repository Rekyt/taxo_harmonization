# Script to create Figure 3 of manuscript to show an extract of the database
# network
# Packages ---------------------------------------------------------------------
library("ggplot2")
library("igraph")
library("ggraph")
library("tidygraph")

# Load data --------------------------------------------------------------------
db_igraph = readRDS("data/data_cleaned/db_igraph.Rds")
db_graph = tidygraph::as_tbl_graph(db_igraph)

biggest_component = db_graph %>%
  activate(nodes) %>%
  mutate(component = group_components()) %>%
  filter(component == 1)

# Plot data --------------------------------------------------------------------

plot_graph_db = ggraph(db_graph, layout = "nicely") + 
  geom_edge_link(
    edge_width = 0.7, colour = "#BBCCD0",
    arrow = arrow(length = unit(3, 'mm'), angle = 15, type = "closed")
  ) + 
  geom_node_point(aes(size = centrality_pagerank(),
                      fill = centrality_pagerank()),
                  shape = 21, colour = "white", stroke = 0.1) +
  geom_node_text(aes(label = name),
                 fontface = "bold", vjust = 2, size = 3, check_overlap = TRUE) +
  scale_fill_viridis_b(name = "Centrality") +
  scale_size_binned(range = c(2, 6), guide = NULL) +
  theme_void()

plot_graph_db

# Save plot --------------------------------------------------------------------

ggsave("figures/fig3_databases_network.png", plot_graph_db, width = 700,
       height = 400, units = "px", dpi = 300, scale = 4)

ggsave("figures/fig3_databases_network.svg", plot_graph_db, width = 700,
       height = 400, units = "px", dpi = 300, scale = 4)
