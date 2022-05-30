list_programs <- the_data |> 
  clean_names() |> 
  select(country_simplified,
         what_is_the_name_of_the_intervention,
         contains("what_type_of_intervention_is_it_choice"),
         contains("who_is_the_target_population_for_the_intervention_choice"),
         contains("which_of_the_following_protective_or_promotive_factors_does_the_intervention_impact_choice_"),
         contains("which_of_the_following_risk_factors_does_the_intervention_impact_choice_")) |>
  pivot_longer(cols = -c("country_simplified","what_is_the_name_of_the_intervention")) |> 
  mutate(name=str_replace_all(name, "what_type_of_intervention_is_it_choice_", "Type__"),
         name=str_replace_all(name, "who_is_the_target_population_for_the_intervention_choice_", "Target__"),
         name=str_replace_all(name, "which_of_the_following_protective_or_promotive_factors_does_the_intervention_impact_choice_", "Protective_Factors__"),
         name=str_replace_all(name, "which_of_the_following_risk_factors_does_the_intervention_impact_choice_", "Risk_factors__"),
  ) |> 
  mutate(value=ifelse(value=="Checked",1,0)) |> 
  separate(name, c("Q", "name"), "__") |> 
  filter(value==1) |> 
  group_by(country_simplified, what_is_the_name_of_the_intervention,
           Q) |> 
  summarize(b=paste(name,collapse=", "))  |> 
 group_split(what_is_the_name_of_the_intervention)

programs_names <- map2(list_programs, seq(1,length(list_programs)), ~list_programs[[.y]][[2,2]]) 
names(list_programs) <- programs_names

col_1 <- pop("Anímate")
col_2 <- pop("El cerebro crece contigo")
col_3 <- pop("Programa Ángel Protector")
col_4 <- pop("Programas para promover la salud mental y prevenir conductas de riesgo en tiempos de covid 19")

arg <- pop(1)
brz <- pop(7)

mex <- test(23)

chl <- list_2[1,2]
# ### list of programs one cell

list_2 <- the_data |> 
  clean_names() |> 
  select(country_simplified,
         what_is_the_name_of_the_intervention,
         contains("what_type_of_intervention_is_it_choice"),
         contains("who_is_the_target_population_for_the_intervention_choice"),
         contains("which_of_the_following_protective_or_promotive_factors_does_the_intervention_impact_choice_"),
         contains("which_of_the_following_risk_factors_does_the_intervention_impact_choice_")) |> 
  group_by(country_simplified) |> 
  mutate(row=paste0("I_",row_number())) |>
  select(country_simplified, row, what_is_the_name_of_the_intervention) |> 
  pivot_wider(names_from = row, values_from = what_is_the_name_of_the_intervention) |> 
  unite("programas", I_1:I_5, na.rm = TRUE, remove = FALSE) 

chl <- list_2[1,2]  


the_data |> clean_names() |> 
  select(interview_number, 
         country_simplified,
         what_is_the_name_of_the_intervention,
         in_what_cities_countries_has_the_intervention_been_implemented) |> 
  write.csv("long_lat.csv")
