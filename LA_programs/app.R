
library(shiny)
library(tidyverse)
library(janitor)
library(shinydashboard)
library(leaflet)
library(reactable)
library(readxl)
library(glue)

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
      tabItem("overview",
      h3("Scoping Review of Youth ATOD Prevention Intervention in Latin America and the Caribbean"),
      h4("Info aqui")
      ),
      tabItem("countries",leafletOutput("mymap")),
      tabItem("databse",
              sliderInput("Filtros", "Slider input:", 1, 100, 50),
              reactableOutput("table")),
      tabItem("literature", 
              h1("text"),
              p("teeeexxtt"),
              box(
                title = "Número de artículos",
                status = "primary", 
                solidHeader = TRUE,
                plotOutput("lit_plot_n", height = 250, 
                           width = 250))
              )
    )
  )
)



server <- function(input, output) {
  
  dataset <- read_xlsx("CICADProject_Dataset_clean.xlsx", sheet = 2) |> 
    clean_names() |> select(interview_number,
                            country_simplified,
                            what_is_the_name_of_the_intervention,
                            please_provide_a_brief_description_of_the_intervention) |> 
    rename(ID=interview_number) |> 
    rename(Pais=country_simplified) |> 
    rename(Programa = what_is_the_name_of_the_intervention) |> 
    rename(Descripcion= please_provide_a_brief_description_of_the_intervention) |> 
    mutate(Programa=str_to_title(Programa)) |> 
    mutate(Descripcion=str_to_sentence(Descripcion))
  
  col <- paste(sep = "<br/>",
                   "<b>Colombia</b>",
                   "Programas:3",
                   "Articulos:2"
  )
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
    
  output$table <- renderReactable({
    reactable(
      dataset,
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
  

# Literature review -------------------------------------------------------

  lit <-read_xlsx("CICADProject_Dataset_clean.xlsx", sheet = 9) 
  
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
}

shinyApp(ui = ui, server = server)
