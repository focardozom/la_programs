# Date: May 16 2020. 
# Author: Francisco Cardozo. 

# Load data programs and interviews ---------------------------------------

the_data <- 
  read_xlsx("CICADProject_Dataset_clean.xlsx", sheet = 2) |> 
  clean_names() %>% 
  filter(!is.na(as.numeric(interview_number))) 
  

dataset <- the_data |> 
  clean_names() |> select(interview_number,
                          country_simplified,
                          what_is_the_name_of_the_intervention,
                          please_provide_a_brief_description_of_the_intervention) |> 
  rename(ID=interview_number) |> 
  rename(Pais=country_simplified) |> 
  rename(Programa = what_is_the_name_of_the_intervention) |> 
  rename(Descripcion= please_provide_a_brief_description_of_the_intervention) |> 
  mutate(Programa=str_to_title(Programa)) |> 
  mutate(Descripcion=str_to_sentence(Descripcion)) |> 
  filter(!is.na(Pais))


# Map labels --------------------------------------------------------------

table_programs <- the_data |> 
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
  select(Pais, Sistema, Programa, Estrategia, Entrenamiento, Otro)

 


# Load data literature review ---------------------------------------------

lit <-read_xlsx("CICADProject_Dataset_clean.xlsx", sheet = 9) |> 
  filter(!is.na(Year))

