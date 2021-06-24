library("shiny")
library("markdown")

navbarPage("taxtool selecter",
           # Introduction panel ----
           tabPanel(
             "Description",
             fluidRow(
               style = "max-height: 100vh; overflow-y: auto;",
               column(width = 2L),
               column(width = 8L,
                      includeMarkdown("app_description.md")),
               column(width = 2L)
             )
           ),
           # Actual Network tab ----
           tabPanel(
             "Network",
             fillPage(
               column(
                 width = 4L,
                 # textOutput("debug_DT_row_number"),
                 DT::dataTableOutput("full_table"),
                 htmlOutput("network_return")
               ),
               column(
                 width = 8L,
                 visNetwork::visNetworkOutput("full_network_interactive",
                                              height = "85vh")
               )
             )
           )
)