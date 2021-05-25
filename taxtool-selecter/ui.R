library(shiny)

fluidPage(
  tabsetPanel(
    # Introduction panel ----
    tabPanel("Introduction", includeMarkdown("introduction.md")),
    tabPanel("Package network",
             sidebarLayout(
               sidebarPanel(
               ),
               mainPanel(
                 plotOutput("pkg_network1"),
                 visNetwork::visNetworkOutput("pkg_network2")
               )
             )
    ),
    tabPanel("Database network",
             sidebarLayout(
               sidebarPanel(
               ),
               mainPanel(
                 plotOutput("DB_network1"),
                 visNetwork::visNetworkOutput("DB_network2")
               )
             )
    ),
    tabPanel("Full Network",
             fluidPage(
               plotOutput("full_network_flat"),
               visNetwork::visNetworkOutput("full_network_interactive")
             )
    )
  )
)
