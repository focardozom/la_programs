to_pop_up <- function(x,y){
  table_programs |> 
    filter(Pais==x) |> 
    pull(y)
}


the_data |> 
  clean_names() |> 
  select(country_simplified,
         what_is_the_name_of_the_intervention,
         contains("what_type_of_intervention_is_it_choice"),
         contains("who_is_the_target_population_for_the_intervention_choice"),
         contains("which_of_the_following_protective_or_promotive_factors_does_the_intervention_impact_choice_"),
         contains("which_of_the_following_risk_factors_does_the_intervention_impact_choice_low_neighborhood_attachment")) |>
  pivot_longer(cols = -c("country_simplified","what_is_the_name_of_the_intervention")) |> 
  mutate(name=str_remove_all(name, "what_type_of_intervention_is_it_choice_"),
         name=str_remove_all(name, "who_is_the_target_population_for_the_intervention_choice_"),
         name=str_remove_all(name, "which_of_the_following_protective_or_promotive_factors_does_the_intervention_impact_choice_"),
         name=str_remove_all(name, "which_of_the_following_risk_factors_does_the_intervention_impact_choice_low_neighborhood_attachment"),
         ) |>
  mutate(value=ifelse(value=="Checked",1,0)) |> 
  filter(value==1)
  

the_data |> 
  clean_names() |> names()
