library("shiny")
library("visNetwork")

load("shiny_data/full_network.Rdata")

# Define server logic for tabs
shinyServer(function(input, output, session) {
  # Full network
  output$full_table <- DT::renderDT(
    all_nodes[, c("Package name","Object type")],
    options = list(target = "row")
  )
  
  output$full_network_interactive <-  visNetwork::renderVisNetwork(
    visNetwork(all_nodes, all_edges) %>% 
      # visGroups(groupname = "db", icon = list(code = "f108",color = "red")) %>%
      visEdges(
        # color = list(highlight = "blue", hover = "blue"), # color is fixed, does not change with group
        arrows = "to"
      ) %>%
      visInteraction(dragNodes = TRUE, 
                     dragView = TRUE, 
                     zoomView = TRUE,
                     selectConnectedEdges = TRUE,
                     hideEdgesOnDrag = TRUE,
                     hideNodesOnDrag = FALSE,
                     navigationButtons = TRUE,
                     tooltipDelay = 300L) %>%
      visOptions(
        # selectedBy = "group", # Add a dropdown menu in which to select the
        # group value that we want highlighted. Any column called "group"
        highlightNearest = list(enabled = TRUE, algorithm = "hierarchical",
                                degree = list(from = 1, to = 1), hover = TRUE),  #, hideColor = "rgba(0,0,0,0)" # secondary edges Are hidden by usinf algo = "hierarchical" and degree = list(from = 1, to = 1)
        nodesIdSelection = TRUE
      ) %>%
      visLayout(randomSeed = 42L)
  )
  
  # observe(output$debug_DT_row_number <- renderText(input$full_table_rows_selected))
  
  observe({
    visNetworkProxy("full_network_interactive") %>%
      visSelectNodes(id = all_nodes[input$full_table_rows_selected, ][["id"]],
                     highlightEdges = FALSE)
  })
  
  
})