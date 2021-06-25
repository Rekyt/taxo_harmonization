library("shiny")
library("markdown")

navbarPage("taxtool selecter",
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
                 width = 4L,
                 # textOutput("debug_DT_row_number"),
                 p("Select node(s) to focus on in the network:"),
                 DT::dataTableOutput("full_table"),
                 htmlOutput("network_return",
                            style = "background: #EEE; border-radius: 3px;
                                     padding: 2px")
               ),
               column(
                 width = 8L,
                 visNetwork::visNetworkOutput("full_network_interactive",
                                              height = "85vh")
               )
             )
           )
)