
library(shiny)
library(shinydashboard)
library(leaflet)
library(reactable)
library(readxl)

ui <- dashboardPage(
  dashboardHeader(title = "Data registry"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Prevention programs", tabName = "programs"),
      menuItem("Data set", 
               tabName = "data_set"),
      menuItem("Literature review", 
               tabName = "lit",
               selectInput("Country", "Country:",
                           c("Col" = "col",
                             "Pr" = "per",
                             "ecu" = "ecu")))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("programs",
                leafletOutput("mymap")
              ),
      tabItem("data_set",
              reactableOutput("table")
              ),
      tabItem("lit", 
              "text"
              )
    )
  )
)



server <- function(input, output) {
  
  dataset <- read_xlsx("CICADProject_Dataset_clean.xlsx", sheet = 2)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      )
  })
    
  output$table <- renderReactable({
    reactable(dataset, groupBy = "Country (simplified)")
  })
}

shinyApp(ui = ui, server = server)
