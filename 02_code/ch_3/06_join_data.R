### Clean working environment ----
remove(list = ls())

### Load packages, define setup and vis theme ----
source("02_code/ch_3/00_pacakges_setup_theme.R")

### Cargar and procesar cada base de datos ----
source("02_code/ch_3/01_import_prepare_legislative_data.R")
source("02_code/ch_3/02_import_prepare_bd_index.R")
source("02_code/ch_3/03_import_prepare_bd_state_gdp.R")
source("02_code/ch_3/04_import_prepare_bd_conapo.R")
source("02_code/ch_3/05_import_prepare_bd_education_urbanization.R")


### Join bd_por_mr and bd_por_rp with bd_index_state, keeping only the data corresponding to female legislators, and create thw new tibbles bd_mr and bd_pr ----

# Data of female legislators elected by majority rule ----
bd_mr <- 
  bd_leg_mr %>%
  full_join(bd_index_state %>% 
              # Keep only observations of majority rule in bd_index_state
              filter(str_detect(electoral_rule, "Major")) %>%
              # Drop some columns
              select(-c(electoral_rule, id_election)),
            # Define columns that will be used to join the tibbles
            by = c("start_yr" = "election_yr", 
                   "state" = "state")) 

# Replace NAs by 0s in ifreg. The NAs correspond to the years in which the electoral law didn't include gender provisions. That's why we replace them with 0s.
bd_mr <-
  bd_mr %>% 
  mutate(ifreg = ifelse(is.na(ifreg), 0, ifreg))

# Data of female legislators elected by proportional representation ----
bd_pr <- 
  bd_leg_pr %>% 
  left_join(bd_index_state %>% 
              # Keep only observations of proportional representation in bd_index_state
              filter(str_detect(electoral_rule, "Prop")) %>%
              # Drop some columns
              select(-c(electoral_rule, id_election)),
            # Define columns that will be used to join the tibbles
            by = c("start_yr" = "election_yr", 
                   "state" = "state")) 
            
# Replace NAs by 0s in ifreg. The NAs correspond to the years in which the electoral law didn't include gender provisions. That's why we replace them with 0s.
bd_pr <-  
  bd_pr %>% 
  mutate(ifreg = ifelse(is.na(ifreg), 0, ifreg))
  

### Create variable that measures how old is the first gender law in each state with respect to the corresponding election ---- 

# Majority rule
bd_mr <- 
  bd_mr %>% 
  group_by(state) %>% 
  # Create yr_first_gender_law which corresponds to the year in which the first gender law was approved
  mutate(yr_first_gender_law = min(law_publication_year, na.rm = T),
         # Replace value of yr_first_gender_law with NAs for those years in which law_publication_year also has value of NA
         yr_first_gender_law = ifelse(is.na(law_publication_year), NA, yr_first_gender_law),
         # Calculate age_first_gender_law, subtracting yr_first_gender_law to start_yr 
         age_first_gender_law = start_yr - yr_first_gender_law,
         # Replace NAs in age_first_gender_law with 0s 
         age_first_gender_law = ifelse(is.na(age_first_gender_law), 0, age_first_gender_law)) %>% 
  ungroup() 

# Proportional representation
bd_pr <- 
  bd_pr %>% 
  group_by(state) %>% 
  # Create yr_first_gender_law which corresponds to the year in which the first gender law was approved
  mutate(yr_first_gender_law = min(law_publication_year, na.rm = T),
         # Replace value of yr_first_gender_law with NAs for those years in which law_publication_year also has value of NA
         yr_first_gender_law = ifelse(is.na(law_publication_year), NA, yr_first_gender_law),
         # Calculate age_first_gender_law, subtracting yr_first_gender_law to start_yr 
         age_first_gender_law = start_yr - yr_first_gender_law,
         # Replace NAs in age_first_gender_law with 0s 
         age_first_gender_law = ifelse(is.na(age_first_gender_law), 0, age_first_gender_law)) %>% 
  ungroup() 

### Join bd_mr and bd_pr with bd_state_gdp and keep result in bd_mr and bd_pr ----

# Data of female legislators elected by majority rule ----
bd_mr <- 
  bd_mr %>%
  left_join(bd_state_gdp,
            # Define columns that will be used to join the tibbles
            by = c("start_yr" = "year", 
                   "state" = "state")) 

# Data of female legislators elected by proportional representation ----
bd_pr <- 
  bd_pr %>%
  left_join(bd_state_gdp,
            # Define columns that will be used to join the tibbles
            by = c("start_yr" = "year", 
                   "state" = "state")) 



### Create variable party_alternation and save it in bd_mr and bd_pr ----

# party_alternation is a dummy variable with value of 1 if *before* the corresponding election (e.g., 2013) there have been party alternation in the state government (e.g., 2012 or before), and 0 in any other case. This last scenario includes those cases in which the alternation of the party in the state government took place in the same election period in which the corresponding congress was elected (e.g., Tabasco en 2013). 

# This variable is constructed using the data of González Ulloa (2017), Alternancia en las elecciones subnacionales en México: ¿síntoma de democratización?, *Estudios Políticos*, Volumen 40, Enero-Abril, páginas 47-69. Url: https://doi.org/10.1016/j.espol.2016.10.010


# Data of female legislators elected by majority rule ----
bd_mr <- 
  bd_mr %>% 
  mutate(party_alternation = case_when(state == "Aguascalientes" & start_yr == 2001 ~ 1,
                                 state == "Baja California" & start_yr == 1992 ~ 1,
                                 state == "Baja California Sur" & start_yr == 2002 ~ 1,
                                 state == "Chiapas" & start_yr == 2001 ~ 1,
                                 state == "Chihuahua" & start_yr == 1995 ~ 1,
                                 state == "Ciudad de México" & start_yr == 2000 ~ 1,
                                 state == "Durango" & start_yr == 2018 ~ 1,
                                 state == "Guanajuato" & start_yr == 1997 ~ 1,
                                 state == "Guerrero" & start_yr == 2008 ~ 1,
                                 state == "Jalisco" & start_yr == 1997 ~ 1,
                                 state == "Michoacán" & start_yr == 2004 ~ 1,
                                 state == "Morelos" & start_yr == 2003 ~ 1,
                                 state == "Nayarit" & start_yr == 2002 ~ 1,
                                 state == "Nuevo León" & start_yr == 2000 ~ 1,
                                 state == "Oaxaca" & start_yr == 2013 ~ 1,
                                 state == "Puebla" & start_yr == 2014 ~ 1,
                                 state == "Querétaro" & start_yr == 2000 ~ 1,
                                 state == "Quintana Roo" & start_yr == 2019 ~ 1,
                                 state == "San Luis Potosí" & start_yr == 2006 ~ 1,
                                 state == "Sinaloa" & start_yr == 2013 ~ 1,
                                 state == "Sonora" & start_yr == 2012 ~ 1,
                                 state == "Tabasco" & start_yr == 2015 ~ 1,
                                 state == "Tamaulipas" & start_yr == 2019 ~ 1,
                                 state == "Tlaxcala" & start_yr == 2001 ~ 1,
                                 state == "Veracruz" & start_yr == 2018 ~ 1,
                                 state == "Yucatán" & start_yr == 2004 ~ 1,
                                 state == "Zacatecas" & start_yr == 2001 ~ 1)) %>% 
  # Replace values posterior to first 1 by 1s
  group_by(state) %>% 
  fill(party_alternation, .direction = "down") %>% 
  ungroup() %>% 
  # Replace NAs by 0s
  mutate(party_alternation = ifelse(is.na(party_alternation), 0, party_alternation)) 

# Data of female legislators elected by proportional representation ----
bd_pr <- 
  bd_pr %>% 
  mutate(party_alternation = case_when(state == "Aguascalientes" & start_yr == 2001 ~ 1,
                                 state == "Baja California" & start_yr == 1992 ~ 1,
                                 state == "Baja California Sur" & start_yr == 2002 ~ 1,
                                 state == "Chiapas" & start_yr == 2001 ~ 1,
                                 state == "Chihuahua" & start_yr == 1995 ~ 1,
                                 state == "Ciudad de México" & start_yr == 2000 ~ 1,
                                 state == "Durango" & start_yr == 2018 ~ 1,
                                 state == "Guanajuato" & start_yr == 1997 ~ 1,
                                 state == "Guerrero" & start_yr == 2008 ~ 1,
                                 state == "Jalisco" & start_yr == 1997 ~ 1,
                                 state == "Michoacán" & start_yr == 2004 ~ 1,
                                 state == "Morelos" & start_yr == 2003 ~ 1,
                                 state == "Nayarit" & start_yr == 2002 ~ 1,
                                 state == "Nuevo León" & start_yr == 2000 ~ 1,
                                 state == "Oaxaca" & start_yr == 2013 ~ 1,
                                 state == "Puebla" & start_yr == 2014 ~ 1,
                                 state == "Querétaro" & start_yr == 2000 ~ 1,
                                 state == "Quintana Roo" & start_yr == 2019 ~ 1,
                                 state == "San Luis Potosí" & start_yr == 2006 ~ 1,
                                 state == "Sinaloa" & start_yr == 2013 ~ 1,
                                 state == "Sonora" & start_yr == 2012 ~ 1,
                                 state == "Tabasco" & start_yr == 2015 ~ 1,
                                 state == "Tamaulipas" & start_yr == 2019 ~ 1,
                                 state == "Tlaxcala" & start_yr == 2001 ~ 1,
                                 state == "Veracruz" & start_yr == 2018 ~ 1,
                                 state == "Yucatán" & start_yr == 2004 ~ 1,
                                 state == "Zacatecas" & start_yr == 2001 ~ 1)) %>% 
  # Replace valores posteriores al primer 1 por 1s
  group_by(state) %>% 
  fill(party_alternation, .direction = "down") %>% 
  ungroup() %>% 
  # Replace NAs por 0s
  mutate(party_alternation = ifelse(is.na(party_alternation), 0, party_alternation)) 


### Join datos de bd_mr and bd_pr with bd_pop and guardar resultado en bd_mr and bd_pr ----

# Data of female legislators elected by majority rule ----
bd_mr <- 
  bd_mr %>%
  left_join(bd_pop,
            # Define columns that will be used to join the tibbles
            by = c("start_yr" = "year", 
                   "state" = "state")) 

# Data of female legislators elected by proportional representation ----
bd_pr <- 
  bd_pr %>%
  left_join(bd_pop,
            # Define columns that will be used to join the tibbles
            by = c("start_yr" = "year", 
                   "state" = "state")) 


### Create state GDP per capita ----

# Data of female legislators elected by majority rule ----
bd_mr <- 
  bd_mr %>% 
  mutate(state_gpd_per_capita = state_gpd/population*1e6, 
         state_gpd_per_capita_rescaled = rescale(state_gpd_per_capita))

# Data of female legislators elected by proportional representation ----
bd_pr <- 
  bd_pr %>% 
  mutate(state_gpd_per_capita = state_gpd/population*1e6, 
         state_gpd_per_capita_rescaled = rescale(state_gpd_per_capita))


### Join data of the percentage of urban population ----

# Data of female legislators elected by majority rule ----
bd_mr <- 
  bd_mr %>%
  left_join(bd_urban_pop,
            # Define columns that will be used to join the tibbles
            by = c("start_yr" = "year", 
                   "state" = "state"))

# Data of female legislators elected by proportional representation ----
bd_pr <- 
  bd_pr %>%
  left_join(bd_urban_pop,
            # Define columns that will be used to join the tibbles
            by = c("start_yr" = "year", 
                   "state" = "state"))


### Join data of the percentage of population with higher ----

# This variable was used in previos versins of this analysis and it is not used for this chapter

# Data of female legislators elected by majority rule ----
bd_mr <- 
  bd_mr %>%
  left_join(bd_high_edu %>% select(-year_info_inegi),
            # Define columns that will be used to join the tibbles
            by = c("start_yr" = "year", 
                   "state" = "state"))

# Data of female legislators elected by proportional representation ----
bd_pr <- 
  bd_pr %>%
  left_join(bd_high_edu %>% select(-year_info_inegi),
            # Define columns that will be used to join the tibbles
            by = c("start_yr" = "year", 
                   "state" = "state"))



### Join data of average years of education ----

# Data of female legislators elected by majority rule ----
bd_mr <- 
  bd_mr %>%
  left_join(bd_edu %>% select(-year_info_inegi),
            # Define columns that will be used to join the tibbles
            by = c("start_yr" = "year", 
                   "state" = "state"))

# Data of female legislators elected by proportional representation ----
bd_pr <- 
  bd_pr %>%
  left_join(bd_edu %>% select(-year_info_inegi),
            # Define columns that will be used to join the tibbles
            by = c("start_yr" = "year", 
                   "state" = "state"))



### Save tibble in files with .csv format ----

bd_mr %>% 
  write_csv("04_generated_data/ch_3/bd_mr.csv")

bd_pr %>% 
  write_csv("04_generated_data/ch_3/bd_pr.csv")
