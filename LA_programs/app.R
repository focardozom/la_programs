
library(shiny)
library(tidyverse)
library(janitor)
library(shinydashboard)
library(leaflet)
library(reactable)
library(readxl)
library(crosstalk)

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
      tabItem("overview", includeMarkdown("text/text_tab1.md")),
      tabItem("countries",leafletOutput("mymap")),
      tabItem("databse",
              selectizeInput("country", "Pais", 
                             c("Multi","Chile","Brazil","Colombia","Peru",
                               "Mexico","El Salvador","Bahamas","Costa Rica","Jamaica",
                               "Ecuador","Panama","Argentina"), 
                             selected = NULL, multiple = TRUE,
                             options = NULL),
              reactableOutput("table")),
      tabItem("literature", 
              includeMarkdown("text/text_tab_literature.md"),
              fluidRow(
                box(
                  title = "Número de artículos",
                  status = "primary", 
                  solidHeader = TRUE,
                  plotOutput("lit_plot_n", height = 250)),
                box(title = "Número de artículos por país",
                    status = "primary", 
                    solidHeader = TRUE,
                    plotOutput("lit_plot_countries", height = 250))
                ),
              reactableOutput("lit_table")
              )
    )
  )
)




server <- function(session, input, output) {


# Import data set ---------------------------------------------------------

  
  source("scripts/load_data.R")
  

# Filter dataset ----------------------------------------------------------
  
  the_data <- reactive({
    
    the_data <-
      if(is.null(input$country)){
      dataset}
    else{  
        dataset |> 
        filter(Pais %in% c(input$country[1],
                           input$country[2],
                           input$country[3],
                           input$country[4],
                           input$country[5],
                           input$country[6],
                           input$country[7],
                           input$country[8],
                           input$country[9],
                           input$country[10],
                           input$country[11],
                           input$country[12]))
        }
    
    })
  

# Reactable ---------------------------------------------------------------

  output$table <- renderReactable({
    reactable(
      the_data(),
      defaultColDef = colDef(
        header = function(value) gsub(".", " ", value, fixed = TRUE),
        cell = function(value) format(value, nsmall = 1),
        minWidth = 70,
        headerStyle = list(background = "#f7f7f8")
      ),
      columns = list(
        Descripcion = colDef(minWidth = 200),
        ID = colDef(minWidth = 20),
        Country = colDef(minWidth = 30) 
      ),
      bordered = TRUE,
      highlight = TRUE
    )
  })  
  
# Map labels --------------------------------------------------------------


  output$mymap <- renderLeaflet({
    leaflet() |> 
      addTiles() |> 
      addMarkers(lng=-74.063644, lat=4.624335, 
      popup=col) |> 
      addMarkers(lng=-99.1269, lat=19.4978, popup="México") |> 
      addMarkers(lng=-64.0000000, lat=-34, popup="Argentina") |> 
      addMarkers(lng=-47.9297200, lat=-15.7797200, popup="Brazil") |> 
      addMarkers(lng=-71.5429688, lat=-35.675148, popup="Chile") |> 
      addMarkers(lng=-77.0282400, lat=-12.0431800 , popup="Peru") |> 
      addMarkers(lng=-88.89653, lat=13.794185, popup="El Salvador") |> 
      addMarkers(lng=-77.39628, lat=25.03428, popup="Bahamas") |> 
      addMarkers(lng=-77.297508, lat=18.109581, popup="Jamaica") |> 
      addMarkers(lng=-77.5000000, lat=-2.0000000, popup="Ecuador") |> 
      addMarkers(lng=-80.782127, lat=8.537981, popup="Panama")
      
  })
    
  

# Literature review -------------------------------------------------------

  
  output$lit_plot_n <- renderPlot({
    lit |> count(Year) |> 
    ggplot(aes(Year, n)) + 
    geom_bar(stat = "identity") +
    scale_y_continuous(breaks = seq(1,20,1), limits = c(0,12)) +
    scale_x_continuous(breaks = seq(1980,2020,2), limits = c(1982,2021)) +
    geom_text(aes(label=n), vjust=-0.1) +
    theme_classic() +
    theme(axis.text.x = element_text(angle=90)) +
    theme(axis.text.y = element_blank()) +
    labs(x="Año de publicación",
         y="Número de artículos",
         title = "")
  })
  output$lit_plot_countries <- renderPlot({
    lit |> select(`Implementation country`) |> 
      mutate(`Implementation country`=str_to_title(str_trim(`Implementation country`))) |> 
      separate_rows(`Implementation country`, sep = ",") |> 
      count(`Implementation country`) |> 
      filter(!is.na(`Implementation country`)) |> 
      ggplot(aes(reorder(`Implementation country`,n), n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x="", y="Número de artículos por año")})
  output$lit_table <- renderReactable({
    reactable(lit,
              defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 1),
                minWidth = 70,
                headerStyle = list(background = "#f7f7f8")
                ),
    columns = list(
      `Intervention description` = colDef(minWidth = 500),
      `Intervention outputs`= colDef(minWidth = 400),
      Year = colDef(minWidth = 50),
      Title = colDef(minWidth = 100),
      Developers = colDef(minWidth = 200),
    `If used, references:`= colDef(minWidth = 300),
    `Additional comments`=colDef(minWidth = 300),
    Duration=colDef(minWidth = 300)),
    bordered = TRUE,
    highlight = TRUE
    )
  })

}


shinyApp(ui = ui, server = server)
