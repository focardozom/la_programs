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

# Argentina ---------------------------------------------------------------

arg <- pop(1)

# Bahamas -----------------------------------------------------------------

bah_1 <- pop("I Totaly Matter!!!")

# Brazil ------------------------------------------------------------------

brz_1 <- pop("#Tamojunto")
brz_2 <- pop("Familias Fortes (Strengthening Families Program)")
brz_3 <- pop("Jogo Elos (Good Behavior Game)")
brz_4 <- pop("PHAVI")
brz_5 <- pop("Programa Educacional de Resistência às Drogas - PROERD (DARE)")
brz_6 <- pop("Projeto Ser")


# Chile -------------------------------------------------------------------

chl_1 <- pop("Continuo Preventivo")
chl_2 <- pop("Saliendo Aprendo")
chl_3 <- pop("Yo Sé lo que Quiero/Mi Mejor Plan")

# Multi

chl_4 <- pop("Familias Unidas")


# Colombia ----------------------------------------------------------------

col_1 <- pop("Anímate")
col_2 <- pop("El cerebro crece contigo")
col_3 <- pop("Programa Ángel Protector")
col_4 <- pop("Programa de prevencion indicada para la cesacion de consumo de cigarillo")
col_5 <- pop("Programas para promover la salud mental y prevenir conductas de riesgo en tiempos de covid 19")

# MULTI
col_6 <- pop("Encaminandolos hacia Buenas Decisiones") #MULTI
col_7 <- pop("Familias Fuertes")
col_8 <- pop("Familias Unidas")
col_9 <- pop("IBEM – Intervención Breve basada en Entrevista Motivacional")
col_10 <- pop("Sanamente y Consentidos")

# Costa Rica --------------------------------------------------------------

cos_1 <- pop("Centros Educativos Frente al Fenómeno de las Drogas (CEPREDE)")

# Ecuador -----------------------------------------------------------------

ecu <- pop("El Arte de Ser Padre en Tiempo de Drogas")


# El Salvador -------------------------------------------------------------

slv <- pop("SIVAR SANO")

# Jamaica -----------------------------------------------------------------

jam <- pop("Talk Di Truth")


# Mexico ------------------------------------------------------------------

mex_1 <- pop("AMBAR")
mex_2 <- pop("Aprendiendo a dar Oportunidades, Habilidades , y Reconocimiento a los Adolescentes y Adultos (AHORA)")
mex_3 <- pop("Creciendo Juntos")
mex_4 <- pop("Dejando Huellitas en Tu Vida")
mex_5 <- pop("Desarollo de Pensamiento Critico sobre Tabaco")
mex_6 <- pop("Empresas Que se Cuidan")
mex_7 <- pop("Para Vivir Sin Adicciones")
mex_8 <- pop("Tomando Buenas Decisiones")

# Multi
mex_9 <- pop("IBEM – Intervención Breve basada en Entrevista Motivacional")


# Panama ------------------------------------------------------------------

pan <- pop("Jovenes Construyendo un Mundo Mejor")

# Peru --------------------------------------------------------------------

per_1 <- pop("Programa escolar de prevención de uso de sustancias psicoactivas")

mex <- test(23)

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


# the_data |> clean_names() |> 
#   select(interview_number, 
#          country_simplified,
#          what_is_the_name_of_the_intervention,
#          in_what_cities_countries_has_the_intervention_been_implemented) |> 
#   write.csv("long_lat.csv")
