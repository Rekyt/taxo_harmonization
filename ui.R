library(shiny)

fluidPage(
  tabsetPanel(
    # Introduction panel ----
    tabPanel("Introduction", includeMarkdown("introduction.md")),
    tabPanel("Package network",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("user_package_selection_ui")
               ),
               mainPanel(
                 plotOutput("pkg_network1"),
                 plotOutput("pkg_network2"),
                 visNetwork::visNetworkOutput("pkg_network3")
               )
             )
    ),
    tabPanel("Database network",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("user_database_selection_ui")
               ),
               mainPanel(
                 plotOutput("DB_network1"),
                 plotOutput("DB_network2"),
                 visNetwork::visNetworkOutput("DB_network3")
               )
             )
    ),
    tabPanel("Full Network",
             fluidPage(
               visNetwork::visNetworkOutput("full_network")
             )
    )
  )
)
