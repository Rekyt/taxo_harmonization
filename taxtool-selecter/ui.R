library(shiny)

fluidPage(
  tabsetPanel(
    # Introduction panel ----
    tabPanel("Introduction", includeMarkdown("introduction.md")),
    tabPanel("Full Network",
             fillPage(
               # selectInput(inputId = "selnodes", label = "Nodes selection", choices = 1:15, multiple = TRUE),
               column(
                 width = 4L,
                 textOutput("debug_DT_row_number"),
                 DT::dataTableOutput("full_table")
               ),
               column(
                 width = 8L,
                 visNetwork::visNetworkOutput("full_network_interactive", height = "800px")
               )
             )
    )
  )
)
