library("shiny")
library("markdown")

navbarPage("taxharmonizexplorer",
           # Introduction panel ----
           tabPanel(
             "Description",
             fluidRow(
               style = "max-height: 100vh; overflow-y: auto;",
               column(width = 1L),
               column(width = 10L,
                      includeMarkdown("app_description.md")),
               column(width = 1L)
             )
           ),
           # Actual Network tab ----
           tabPanel(
             "Network",
             fluidRow(
               style = "max-height: 100vh; overflow-y: auto;",
               column(
                 width = 5L,
                 # textOutput("debug_DT_row_number"),
                 htmlOutput("network_return",
                            style = "background: #EEE; border-radius: 3px;
                                     padding: 2px"),
                 p("Click on one (several) node(s) to highlight it (them) in the network:"),
                 DT::dataTableOutput("full_table")
               ),
               column(
                 width = 7L,
                 visNetwork::visNetworkOutput("full_network_interactive",
                                              height = "85vh")
               )
             )
           )
)