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
  
  
  # Additional Legend for Edges (not included by default)
  ledges <- data.frame(
    color = c("#998EC3", "steelblue", "#B35806"),
    label = c("package\ndepends on", "package\naccesses",
              "database\npopulates"),
    arrows = c("to", "to", "to"), 
    font.align = "middle")
  
  
  output$full_network_interactive <-  visNetwork::renderVisNetwork(
    visNetwork(all_nodes, all_edges) %>%
      # Databases
      visGroups(groupname = "database", shape = "dot", color = list(
        background = "#F1A340",
        border = "#B35806",
        highlight = list(
          background = "#B35806",
          border = "#F1A340"
        ))) %>%
      
      # Packages
      visGroups(groupname = "package", shape = "dot", color = list(
        background = "#998EC3",
        border = "#542788",
        highlight = list(
          background = "#542788",
          border = "#998EC3"
        )),
        font = list(face = "Courier New")) %>%
      
      # Interaction Options
      visInteraction(dragNodes = TRUE, 
                     dragView = TRUE, 
                     zoomView = TRUE,
                     selectConnectedEdges = TRUE,
                     hideEdgesOnDrag = TRUE,
                     hideNodesOnDrag = FALSE,
                     navigationButtons = TRUE,
                     tooltipStay = 60000L,
                     tooltipDelay = 300L) %>%
      
      # Other options
      visOptions(
        # Add a dropdown menu in which to select the group value that we want
        # highlighted. Any column called "group"
        # selectedBy = "group", 
        highlightNearest = list(
          enabled = TRUE, algorithm = "hierarchical",
          degree = list(from = 1, to = 1), hover = FALSE
        ),
        nodesIdSelection = TRUE
      ) %>%
      
      # Legend options
      visLegend(
        enabled = TRUE,
        useGroups = TRUE,
        zoom = FALSE,
        addEdges = ledges
      ) %>%
      visLayout(randomSeed = 42L)
  )
  
  # Debugging code
  # observe(output$debug_DT_row_number <- renderText(input$full_table_rows_selected))
  
  observe({
    visNetworkProxy("full_network_interactive") %>%
      visSelectNodes(id = all_nodes[input$full_table_rows_selected, ][["id"]],
                     highlightEdges = FALSE)
  })
  
  
})