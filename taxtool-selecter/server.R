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
  
  
  # legend preparation
  ledges <- data.frame(color = c("97C2FC", "lawngreen"), 
                       label = c("Gives to", "Takes from"), arrows = c("to", "from"), 
                       font.align = "top")
  
  
  output$full_network_interactive <-  visNetwork::renderVisNetwork(
    visNetwork(all_nodes, all_edges) %>% 
      
      visGroups(groupname = "db", shape = "dot", color = list(
        background = "lawngreen",
        border = "limegreen",
        highlight = list(
          background = "lawngreen",
          border = "white"
        ))) %>%
      visGroups(groupname = "package", shape = "dot", color = list(
        background = "#97C2FC",
        border = "#2B7CE9",
        highlight = list(
          background = "#97C2FC",
          border = "white"
        ))) %>%
      
      visEdges(
        arrows = "to"
      ) %>%
      
      visInteraction(dragNodes = TRUE, 
                     dragView = TRUE, 
                     zoomView = TRUE,
                     selectConnectedEdges = TRUE,
                     hideEdgesOnDrag = TRUE,
                     hideNodesOnDrag = FALSE,
                     navigationButtons = TRUE,
                     tooltipStay = 60000L,
                     tooltipDelay = 300L) %>%
      visOptions(
        # selectedBy = "group", # Add a dropdown menu in which to select the group value that we want highlighted. Any column called "group"
        highlightNearest = list(enabled = TRUE, algorithm = "hierarchical",
                                degree = list(from = 1, to = 1), hover = FALSE),  #, hideColor = "rgba(0,0,0,0)" # secondary edges are hidden by usinf algo = "hierarchical" and degree = list(from = 1, to = 1)
        nodesIdSelection = TRUE
      ) %>%
      visLegend(
        enabled = TRUE,
        useGroups = TRUE,
        zoom = FALSE,
        addEdges =  ledges) %>%
      visLayout(randomSeed = 42L)
  )
  
  # observe(output$debug_DT_row_number <- renderText(input$full_table_rows_selected))
  
  observe({
    visNetworkProxy("full_network_interactive") %>%
      visSelectNodes(id = all_nodes[input$full_table_rows_selected, ][["id"]],
                     highlightEdges = FALSE)
  })
  
  
})