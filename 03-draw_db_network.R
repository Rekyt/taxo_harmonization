# Script to create Figure 3 of manuscript to show an extract of the database
# network
# Packages ---------------------------------------------------------------------
library("ggplot2")
library("igraph")
library("ggraph")
library("tidygraph")

# Load data --------------------------------------------------------------------
db_igraph = readRDS("data_cleaned/db_igraph.Rds")
db_graph = tidygraph::as_tbl_graph(db_igraph)

biggest_component = db_graph %>%
  activate(nodes) %>%
  mutate(component = group_components()) %>%
  filter(component == 1)

# Plot data --------------------------------------------------------------------

plot_graph_db = ggraph(biggest_component, layout = "kk") + 
  geom_edge_link(edge_width = 2/3, colour = "#88b4da",
                 arrow = arrow(length = unit(2.8, 'mm'), angle = 12,
                               type = "closed")) + 
  geom_node_point(size = 3, shape = 21, colour = "white", fill = "black") +
  geom_node_text(aes(label = name), family = "Consolas",
                 fontface = "bold", vjust = 2, size = 4) +
  theme_void()

# Save plot --------------------------------------------------------------------

ggsave("figures/fig3_databases_network.png", plot_graph_db, width = 700,
       height = 400, units = "px")
