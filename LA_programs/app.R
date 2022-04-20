
library(shiny)
library(shinydashboard)
library(leaflet)

ui <- dashboardPage(
  dashboardHeader(title = "Data registry"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prevention programs", tabName = "programs"),
      menuItem("Data set", tabName = "data_set"),
      menuItem("Literature review", tabName = "lit")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("programs",
                leafletOutput("mymap")
              ),
      tabItem("data_set",
              verbatimTextOutput("Data set of programs"),
              downloadButton("downloadCsv", "Download as dataset?")
      ),
      tabItem("lit", 
              verbatimTextOutput("Literature Review here"))
    )
  )
)



server <- function(input, output) {

  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      )
  })
    
  
}

shinyApp(ui = ui, server = server)
