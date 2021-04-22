library(shiny)

# Define server logic for tabs
shinyServer(function(input, output, session) {
  # 1 packagee network tab ----
  output$user_package_selection_ui <- renderUI({
    checkboxGroupInput(
      "shiny_user_selection",
      label = "Package selection",
      choices = union(taxo_df$pkg, taxo_df$package),
      selected = union(taxo_df$pkg, taxo_df$package)[c(1:2, 9:15)]
    )
  })
  # Package network 1 ----
  output$pkg_network1 <- renderPlot({
    taxo_df_plot <- taxo_df %>% filter(pkg %in% input$shiny_user_selection & package %in% input$shiny_user_selection)
    inc_pkg_plot <- inc_pkg %>% filter(`Package Name` %in% union(taxo_df_plot$pkg, taxo_df_plot$package))
    
    taxo_graph = igraph::graph_from_data_frame(
      taxo_df_plot[, c(2, 1, 3)], vertices = inc_pkg)
    taxo_graph %>%
      ggraph(layout = "igraph", algorithm = "nicely") +
      geom_edge_link(
        arrow = arrow(type = "closed", length = unit(4, "mm"), angle = 7),
        alpha = 1/2
      ) +
      geom_node_point(
        aes(
          fill = "Is this package central in taxonomic harmonization workflow?"
        ),
        shape = 21, color = "white", size = 3) +
      geom_node_label(aes(label = name), family = "Consolas", repel = TRUE) +
      theme_void() +
      theme(legend.position = "top")
  })
  
  # Package network 2 ----
  output$pkg_network2 <- renderPlot({
    taxo_df_plot <- taxo_df %>% filter(pkg %in% input$shiny_user_selection & package %in% input$shiny_user_selection)
    inc_pkg_plot <- inc_pkg %>% filter(`Package Name` %in% union(taxo_df_plot$pkg, taxo_df_plot$package))
    
    taxo_graph = igraph::graph_from_data_frame(
      taxo_df_plot[, c(2, 1, 3)], vertices = inc_pkg_plot)
    taxo_graph %>%
      ggraph(layout = "igraph", algorithm = "nicely") +
      geom_edge_link(
        arrow = arrow(type = "closed", length = unit(4, "mm"), angle = 7),
        alpha = 1/2
      ) +
      geom_node_point(
        aes(
          fill = "Is this package central in taxonomic harmonization workflow?"
        ),
        shape = 21, color = "white", size = 3) +
      geom_node_label(aes(label = name), family = "Consolas", repel = TRUE) +
      theme_void() +
      theme(legend.position = "top")
  })
  
  
  
  
  
  
  
  
  
  
  # 2 database network ----
  output$user_database_selection_ui <- renderUI({
    checkboxGroupInput(
      "user_database_selection",
      label = "Database selection",
      choices = all_db$db_list,
      selected = all_db$db_list
    )
  })
  
  # Database network 1 ----
  output$DB_network1 <- renderPlot({
    db_links_plot <- db_links %>% filter(source_db %in% input$user_database_selection & target_db %in% input$user_database_selection)
    all_db_plot <- all_db %>% filter(db_list %in% union(db_links_plot$source_db, db_links_plot$target_db))
    
    db_graph = igraph::graph_from_data_frame(db_links_plot, vertices = all_db)
    db_graph %>%
      tidygraph::as_tbl_graph() %>%
      ggraph(layout = "igraph", algorithm = "nicely") +
      geom_edge_link(arrow = arrow(type = "closed",
                                   length = unit(4, "mm"), angle = 7), alpha = 1/2) +
      geom_node_point(shape = 21, color = "white", fill  = "black") +
      geom_node_text(aes(label = name), check_overlap = TRUE, repel = TRUE) +
      theme_void()
  })
  
  
  # Database network 2 ----
  output$DB_network2 <- renderPlot({
    db_links_plot <- db_links %>% filter(source_db %in% input$user_database_selection & target_db %in% input$user_database_selection)
    all_db_plot <- all_db %>% filter(db_list %in% union(db_links_plot$source_db, db_links_plot$target_db))
    
    db_graph = igraph::graph_from_data_frame(db_links_plot, vertices = all_db_plot)
    db_graph %>%
      tidygraph::as_tbl_graph() %>%
      ggraph(layout = "igraph", algorithm = "nicely") +
      geom_edge_link(arrow = arrow(type = "closed",
                                   length = unit(4, "mm"), angle = 7), alpha = 1/2) +
      geom_node_point(shape = 21, color = "white", fill  = "black") +
      geom_node_text(aes(label = name), check_overlap = TRUE, repel = TRUE) +
      theme_void()
  })
  # 3 Full network tab ----
  output$full_network <- renderPlot(readRDS("./figures/full_network_plot.rds"))
  
})