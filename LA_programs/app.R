
library(shiny)
library(tidyverse)
library(janitor)
library(shinydashboard)
library(leaflet)
library(reactable)
library(readxl)
library(crosstalk)

source("scripts/Functions.R")
source("scripts/load_data.R")
source("scripts/labels.R")

ui <- dashboardPage(
  dashboardHeader(title = "Data Registry"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview"),
      menuItem("Countries", tabName = "countries"),
      menuItem("Database", tabName = "databse"),
      menuItem("Literature review", tabName = "literature"),
      menuItem("Contact", tabName = "contact")
    )
  ),
  dashboardBody(
    tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
    tabItems(
      
      tabItem("overview", 
              includeMarkdown("text/text_tab1.md")),
      tabItem("countries",
              leafletOutput("mymap")),
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
              reactableOutput("the_programs"),
              h1("Literature summary"),
              reactableOutput("lit_table")
              ),
      tabItem("contact", 
              p("Contact:"),
              p("perla@miami.edu"))
    )
  )
)

# Import data set ---------------------------------------------------------



# Functions ---------------------------------------------------------------



server <- function(session, input, output) {


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


# Arg ---------------------------------------------------------------------

    addMarkers(lng=-64.0000000, lat=-34, popup=arg) |> 
      
# Bahamas -----------------------------------------------------------------

    addMarkers(lng=-77.39628, lat=25.03428, popup=bah_1) |> 
      

# Brz ---------------------------------------------------------------------

    addMarkers(lng=-47.9297200, lat=-15.7797200, popup=brz_1) |> 
    addMarkers(lng=-48.9297200, lat=-15.7797200, popup=brz_2) |> 
    addMarkers(lng=-49.9297200, lat=-15.7797200, popup=brz_3) |> 
    addMarkers(lng=-50.9297200, lat=-15.7797200, popup=brz_4) |> 
    addMarkers(lng=-51.9297200, lat=-15.7797200, popup=brz_5) |> 
    addMarkers(lng=-52.9297200, lat=-15.7797200, popup=brz_6) |> 
      
      

# Chl ---------------------------------------------------------------------

    addMarkers(lng=-71.5429688, lat=-35.675148, popup=chl_1) |> 
    addMarkers(lng=-71.5429688, lat=-35.775148, popup=chl_2) |> 
    addMarkers(lng=-71.5429688, lat=-35.875148, popup=chl_3) |> 
      
      

# col ---------------------------------------------------------------------

      addMarkers(lng=-74.063644, lat=4.624335, popup=col_1) |> 
      addMarkers(lng=-75.590553, lat=6.230833, popup=col_2) |> 
      addMarkers(lng=-76.638565, lat=3.359889, popup=col_3) |> 
      addMarkers(lng=-76.063644, lat=4.624335, popup=col_4) |> 
      addMarkers(lng=-74.19904, lat=11.24079, popup=col_5) |> 
      addMarkers(lng=-76.063644, lat=6.624335, popup=col_6) |> 
      addMarkers(lng=-75.675690, lat=4.535000, popup=col_7) |> 
      addMarkers(lng=-75.675690, lat=4.535000, popup=col_8) |> 
      addMarkers(lng=-74.796387, lat=10.963889, popup=col_9) |> 
      

# Costa Rica --------------------------------------------------------------

    addMarkers(lng=-84.087502, lat=9.934739, popup=cos_1) |> 
      
# Ecuador -----------------------------------------------------------------

    addMarkers(lng=-77.5000000, lat=-2.0000000, popup="Ecuador") |> 
      
# El Salvador -------------------------------------------------------------

    addMarkers(lng=-88.89653, lat=13.794185, popup="El Salvador") |> 
      
# Jamaica -----------------------------------------------------------------

    addMarkers(lng=-77.297508, lat=18.109581, popup="Jamaica") |> 
      

# Mexico ------------------------------------------------------------------

    addMarkers(lng=-99.1269, lat=19.4978, popup=mex) |> 
      

# Panama ------------------------------------------------------------------

    addMarkers(lng=-80.782127, lat=8.537981, popup="Panama") |>  
    

# Peru --------------------------------------------------------------------


      addMarkers(lng=-77.0282400, lat=-12.0431800 , popup="Peru")
      
      
  })
    
  output$the_programs <- renderReactable({
    reactable(table_programs)
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
        Year = colDef(minWidth = 60),
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
