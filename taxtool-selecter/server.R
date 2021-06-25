library("shiny")
library("visNetwork")

load("shiny_data/full_network.Rdata")

# Define server logic for tabs
shinyServer(function(input, output, session) {
  # Full network
  output$full_table <- DT::renderDT(
    all_nodes[, c("Node Name","Object type")],
    options = list(target = "row")
  )
  
  
  # Additional Legend for Edges (not included by default)
  ledges <- data.frame(
    color = c("#998EC3", "steelblue", "#B35806"),
    label = c("package\ndepends on", "package\naccesses",
              "database\npopulates"),
    arrows = c("to", "to", "to"), 
    font.align = "middle")
  
  # Network visualization
  output$full_network_interactive <-  visNetwork::renderVisNetwork(
    visNetwork(
      all_nodes, all_edges,
      # Network title
      main = "Relationships between taxonomic R packages and databases"
    ) %>%
      # Databases
      visGroups(groupname = "database", shape = "triangle", color = list(
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
        font = list(face = "Courier New", multi = TRUE)) %>%
      
      # Interaction Options
      visInteraction(dragNodes = TRUE, 
                     dragView = TRUE, 
                     zoomView = TRUE,
                     selectConnectedEdges = TRUE,
                     hideEdgesOnDrag = TRUE,
                     hideNodesOnDrag = FALSE,
                     navigationButtons = TRUE,
                     keyboard = TRUE,
                     tooltipStay = 3000L,
                     tooltipDelay = 100L) %>%
      
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
      visLayout(randomSeed = 42L) %>%
      visEvents(
        # Functions to get information about selected node
        selectNode = "function(nodes) {
        Shiny.onInputChange('current_node_id', nodes);
      ;}",
        deselectNode = "function(nodes) {
          Shiny.setInputValue('current_node_id', null)
      ;}") %>%
      visPhysics(stabilization = FALSE)
  )
  
  # Select nodes in network based on selection in DT
  observe({
    visNetworkProxy("full_network_interactive") %>%
      visSelectNodes(id = all_nodes[input$full_table_rows_selected, ][["id"]],
                     highlightEdges = FALSE)
  })
  
  # Render information about selected node
  output$network_return <- renderUI({
    
    info <- ifelse(
      is.null(input$current_node_id), "none",
      ifelse(
        is.null(input$current_node_id) |
          is.null(input$current_node_id$nodes[[1]]),
        "none",
        subset(all_nodes, id == input$current_node_id$nodes[[1]])[["html_info"]]
      )
    )
    
    HTML("<b>Selected Node Information</b><br />",
         info)
  })
})