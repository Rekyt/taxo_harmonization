library("shiny")

navbarPage("taxtool selecter",
           # Introduction panel ----
           tabPanel(
             "Description",
             includeMarkdown("app_description.md")
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