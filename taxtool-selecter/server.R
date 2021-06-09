library("shiny")
library("visNetwork")

load("shiny_data/full_network.Rdata")

# Define server logic for tabs
shinyServer(function(input, output, session) {
  # Full network
  output$full_table <- DT::renderDT(
    all_nodes[, c("label","group")],
    options = list(target = "row")
  )
  
  output$full_network_interactive <-  visNetwork::renderVisNetwork(
    visNetwork(all_nodes, all_edges) %>% 
      # visGroups(groupname = "db", icon = list(code = "f108",color = "red")) %>%
      visEdges(arrows = "to") %>%
      visInteraction(dragNodes = TRUE, 
                     dragView = TRUE, 
                     zoomView = TRUE,
                     hideEdgesOnDrag = TRUE,
                     hideNodesOnDrag = FALSE,
                     navigationButtons = TRUE,
                     tooltipDelay = 300L) %>%
      visOptions(
        # selectedBy = "group", # Add a dropdown menu in which to select the
        # group value that we want highlighted. Any column called "group"
        highlightNearest = TRUE,
        nodesIdSelection = TRUE
      ) %>%
      visLayout(randomSeed = 42L)
  )
  
  observe(output$debug_DT_row_number <- renderText(input$full_table_rows_selected))
  
  observe({
    selnodes <- input$full_table_rows_selected
    visNetworkProxy("full_network_interactive") %>%
      visSelectNodes(id = pull(all_nodes[selnodes, "id"]))
    # visSelectNodes(id = input$selected_nodes)
    # visFocus(id = selnodes)
  })
  
  
})