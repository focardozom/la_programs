#Clean data

library(janitor)
library(tidyverse)
library(stringr)


the_data
read_xlsx("CICADProject_Dataset_clean.xlsx", sheet = 2) |> 
  clean_names() |>  select(country_simplified,
                           what_type_of_intervention_is_it_choice_program,
                           what_type_of_intervention_is_it_choice_strategy_practice,
                           what_type_of_intervention_is_it_choice_training,
                           what_type_of_intervention_is_it_choice_system,
                           what_type_of_intervention_is_it_choice_other_please_specify) |> 
  pivot_longer(-country_simplified) |> 
  mutate(name=str_remove_all(name, "what_type_of_intervention_is_it_choice_")) |> 
  mutate(value=ifelse(value=="Checked", 1, 0)) |> 
  group_by(country_simplified, name) |> 
  summarise(total=sum(value)) |> 
  pivot_wider(names_from = name, 
              values_from = total, 
              id_cols = country_simplified) |> 
  rename(Pais=country_simplified, 
         Otro=other_please_specify, 
         Programa= program,
         `Estrategia`= strategy_practice,
         Sistema=system,
         Entrenamiento=training) |> 
  select(Pais, Sistema, Programa, Estrategia, Entrenamiento, Otro) |> 
  reactable()

  
  

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

lit |> select(`Implementation country`) |> 
  mutate(`Implementation country`=str_to_title(str_trim(`Implementation country`))) |> 
  separate_rows(`Implementation country`, sep = ",") |> 
  count(`Implementation country`) |> 
  filter(!is.na(`Implementation country`)) |> 
  ggplot(aes(reorder(`Implementation country`,n), n)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x="", y="Número de artículos")
  
