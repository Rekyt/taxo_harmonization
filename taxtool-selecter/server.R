library("shiny")
library("visNetwork")
library("dplyr")
library("ggraph")

taxonomy_pkg_graph = readRDS("shiny_data/taxonomy_pkg_graph.Rds")
db_graph           = readRDS("shiny_data/db_igraph.Rds")
all_graph          = readRDS("shiny_data/full_network.Rds")

# Define server logic for tabs
shinyServer(function(input, output, session) {
  # Package Networks -----------------------------------------------------------
  # Package network 1 (flat ggraph)
  output$pkg_network1 <- renderPlot({
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
  })
  
  # Package network 2 (Interactive network visulization)
  output$pkg_network2 <- visNetwork::renderVisNetwork({
    visNetwork::visIgraph(taxonomy_pkg_graph)
  })
  
  # Database Networks ----------------------------------------------------------
  # Database network 1 (flat network)
  output$DB_network1 <- renderPlot({
    db_graph %>%
      tidygraph::as_tbl_graph() %>%
      ggraph(layout = "igraph", algorithm = "nicely") +
      geom_edge_link(arrow = arrow(type = "closed",
                                   length = unit(4, "mm"), angle = 7), alpha = 1/2) +
      geom_node_point(shape = 21, color = "white", fill  = "black") +
      geom_node_text(aes(label = name), check_overlap = TRUE, repel = TRUE) +
      theme_void()
  })
  
  # Database network 2 (interactive network)
  output$DB_network2 <- visNetwork::renderVisNetwork({
    visNetwork::visIgraph(db_graph)
  })
  
  # Full (Pkgs & DBs) network --------------------------------------------------
  output$full_network_flat <- renderPlot({
    all_graph %>%
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
            family = ifelse(node_type == "package", "mono", "sans")
        ), check_overlap = TRUE, repel = TRUE
      ) +
      scale_shape_manual(values = c(db = 22, package = 21),
                         labels = c(db = "Database", package = "Package"),
                         name = "Node Type") +
      scale_edge_color_brewer(type = "qual",
                              name = "Link Type") +
      theme_void() +
      theme(legend.position = "top")
  })
  output$full_network_interactive <-  visNetwork::renderVisNetwork(
    visNetwork::visIgraph(all_graph)
  )
})