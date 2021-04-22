library(shiny)

fluidPage(
  tabsetPanel(
    tabPanel("Package network",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("user_package_selection_ui")
               ),
               mainPanel(
                 plotOutput("pkg_network1"),
                 plotOutput("pkg_network2")
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
                 plotOutput("DB_network2")
               )
             )
    ),
    tabPanel("Full Network",
             fluidPage(
               plotOutput("full_network")
             )
    )
  )
)
