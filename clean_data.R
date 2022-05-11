#Clean data

library(janitor)
library(tidyverse)
library(stringr)
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
  mutate(Descipcion=str_to_sentence(Descripcion))

dataset$country_simplified

lit <-read_xlsx("CICADProject_Dataset_clean.xlsx", sheet = 9) 

lit |> count(Year) |> 
ggplot( aes(Year, n)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(breaks = seq(1,20,1), limits = c(0,12)) +
  scale_x_continuous(breaks = seq(1980,2020,2), limits = c(1982,2021)) +
  geom_text(aes(label=n), vjust=-0.1) +
  theme_void() +
  theme(axis.text.x = element_text(angle=90)) +
  labs(x="Año de publicación",
       title = "Número de articulos")
  
