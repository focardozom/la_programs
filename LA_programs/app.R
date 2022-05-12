
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
      tabItem("overview",
      h3("Scoping Review of Youth ATOD Prevention Intervention in Latin America and the Caribbean"),
      h4("Objective"),
      p("Consistently high rates of alcohol, tobacco, and other drug (ATOD) use by youth in Latin America and the Caribbean have called for the identification of preventive interventions that address this public health problem. In response, the Inter-American Drug Abuse Control Commission of the Organization of American States (CICAD-OAS) sponsored a scoping review of the literature and interviews with key informants to identify behavioral interventions that address youth ATOD use in Latin America and the Caribbean (LAC)."),
      h4("Methods"),
      p("A scoping review of published articles that met comprehensive search criteria was conducted via multiple bibliographic databases. The review was supplemented by key informant interviews with ATOD prevention experts throughout LAC. "),
      h4("Results"),
      p("Despite many articles that met the initial search criteria, only 57 articles contained information on youth ATOD preventive interventions, with 39 unique interventions being identified. Results of interviews with 47 key informants identified an additional 38 youth ATOD preventive interventions across 29 LAC countries for a total of 77 unique interventions. Fifty-one of these interventions were developed domestically and 26 were adapted from interventions outside the LAC region."),
      h4("Conclusions"),
      p("The growing development and diffusion of preventive interventions for youth ATOD use in LAC countries calls for a standardized approach to collect, disseminate, and use information on these interventions for informed prevention planning. This investigation is the first of its kind to comprehensively review these interventions and thus provides an important step forward towards a science-based approach to reducing youth ATOD use in the region.")
      ),
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
              h2("Scoping Review of the Literature"),
              p("We conducted a scoping literature review in six sequential stages (11): (1) clarifying and linking the purpose and research question; (2) balancing feasibility with breadth and comprehensiveness of the review; (3) using an iterative approach where the search strategy is continuously reviewed and refined for improvement; (4) extracting the data from the literature; (5) incorporating a numerical summary, reporting results, and assessing the implications of study findings; and (6) consulting with stakeholders."),
              h4("Search Process and Inclusion Criteria"),
              p("Literature searches were conducted in the following electronic databases: PubMed, EBSCOHost (choosing Academic Premier, PsycINFO and ERIC), LILACS, and SciELO. The search criteria required that articles had to (a) be focused on an EPI from a LAC country; (b) be in English, Spanish, Portuguese, or French; (c) have a focus on primary prevention (12) (i.e., activities that result in abstinence, delayed onset, or reduction in the likelihood of ATOD use) or health promotion (i.e., positive prosocial activities that offset or mitigate the likelihood of ATOD use); (d) focus on ATOD use as a final outcome of the intervention; and (e) target children, youth, or young adults between the ages of 6 and 21 years. The search strategy was adjusted for each country, with the inclusion of more complex search terms for countries with a higher academic publication output and simpler strategies for countries with a smaller output. 
                Eligible preventive interventions had to meet the definition of a program, environmental strategy, or prevention implementation system. A program was defined as a manualized curriculum administered by trained professionals in a specific setting and usually requiring certification. An environmental strategy was defined as a nongovernmental set of coordinated activities targeted to a specific population, typically not requiring extensive training or certification. Prevention systems were defined as an integrated set of preventive interventions or implementation-related activities (e.g., program selection, fidelity monitoring) conducted in a broad setting (e.g., community, workplace) that coordinate prevention efforts in groups of people. Broad governmental prevention policies at a national or local level (e.g., minimum age for alcohol use laws, single cigarette sale regulations, alcohol distribution restrictions) were excluded from the search, with exceptions for countries that implemented specific EPIs through policy initiatives. EPIs that were exclusively educational or informative (e.g., public lecture) without a specified behavioral change component also were excluded from the search."),
              h4("Literature Search Data Synthesis"),
              p("Articles identified using the literature search criteria were synthesized in a coding and extraction spreadsheet. First, article titles and abstracts were scanned to assess if the inclusion-exclusion criteria were met. Articles that passed this stage then had their full text scanned for confirmation of compliance with the inclusion-exclusion criteria. Finally, articles that still were approved were subject to data extraction where coders thoroughly read the full text of the publications and extracted key information about the EPI. The PRISMA (Preferred Reporting Items for Systematic Reviews and Meta-Analyses; 13)"),
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
  output$lit_plot_countries <- renderPlot({
    lit |> select(`Implementation country`) |> 
      mutate(`Implementation country`=str_to_title(str_trim(`Implementation country`))) |> 
      separate_rows(`Implementation country`, sep = ",") |> 
      count(`Implementation country`) |> 
      filter(!is.na(`Implementation country`)) |> 
      ggplot(aes(reorder(`Implementation country`,n), n)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x="", y="Número de artículos")})
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
