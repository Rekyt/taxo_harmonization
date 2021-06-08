library("shiny")
library("visNetwork")

load("shiny_data/full_network.Rdata")

# Define server logic for tabs
shinyServer(function(input, output, session) {
  # Full network
  output$full_network_interactive <-  visNetwork::renderVisNetwork(
    visNetwork(all_nodes, all_edges, height = "2000px", width = "100%") %>% 
      visEdges(arrows = "to") %>%
      visInteraction(dragNodes = TRUE, 
                     dragView = TRUE, 
                     zoomView = TRUE,
                     hideEdgesOnDrag = TRUE,
                     hideNodesOnDrag = FALSE,
                     navigationButtons = TRUE,
                     tooltipDelay = 300L) %>%
      visOptions(selectedBy = "node_type", 
                 highlightNearest = TRUE, 
                 nodesIdSelection = TRUE) %>%
      visLayout(randomSeed = 42L)
  )
})