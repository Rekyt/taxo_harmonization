library(shiny)

fluidPage(
  tabsetPanel(
    # Introduction panel ----
    tabPanel("App Description", includeMarkdown("app_description.md")),
    tabPanel("Network",
             fillPage(
               # selectInput(inputId = "selnodes", label = "Nodes selection", choices = 1:15, multiple = TRUE),
               column(
                 width = 4L,
                 # textOutput("debug_DT_row_number"),
                 DT::dataTableOutput("full_table"),
                 # selectInput("selected_nodes", "Choose a node", c("rcol","rebird","rentrez"), multiple = TRUE)
               ),
               column(
                 width = 8L,
                 visNetwork::visNetworkOutput("full_network_interactive", height = "800px")
               )
             )
    )
  )
)
