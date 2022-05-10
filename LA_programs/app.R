
library(shiny)
library(shinydashboard)
library(leaflet)
library(reactable)
library(readxl)

ui <- dashboardPage(
  dashboardHeader(title = "Data registry"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Countries", tabName = "countries"),
      menuItem("Database", tabName = "databse"),
      menuItem("Literature review", tabName = "literature")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("overview"),
      tabItem("countries",leafletOutput("mymap")),
      tabItem("databse",reactableOutput("table")),
      tabItem("literature", 
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
