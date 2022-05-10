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
