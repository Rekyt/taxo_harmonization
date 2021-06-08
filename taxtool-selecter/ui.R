library(shiny)

fluidPage(
  tabsetPanel(
    # Introduction panel ----
    tabPanel("Introduction", includeMarkdown("introduction.md")),
    tabPanel("Full Network",
               visNetwork::visNetworkOutput("full_network_interactive")
    )
  )
)
