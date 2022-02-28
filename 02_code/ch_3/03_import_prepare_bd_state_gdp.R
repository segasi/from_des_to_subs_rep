### Load packages, define setup and vis theme ----
source("02_code/ch_3/00_pacakges_setup_theme.R")

### Import data ----
bd_state_gdp <- 
  read_excel("01_data/ch_3/inegi/pibe/PIBER_2.xlsx",
             skip = 4,
             range = "a5:ao43") 


### Filter/eliminate observations that don't correspond to states ----
bd_state_gdp <- 
  bd_state_gdp %>% 
  filter(!is.na(`1980`), 
         !str_detect(Concepto, "___"),
         !str_detect(Concepto, "D.21"),
         !str_detect(Concepto, "states")) 

### Rename variable Concepto ----
bd_state_gdp <- 
  bd_state_gdp %>% 
  rename(state = Concepto)


### Change data structure from wide to long ----
bd_state_gdp <- 
  bd_state_gdp %>%
  pivot_longer(-state,
               names_to = "year",
               values_to = "state_gpd")


### Transform variable type of year to be numeric----
bd_state_gdp <- 
  bd_state_gdp %>%
  mutate(year = str_replace(year, "R", ""),
         year = str_replace(year, "P", ""),
         year = as.numeric(year)) 

### Create state_gpd_rescaled, a rescaled version of state_gpd with values that range from 0 to 1 ----

# This transformation is needed because the original unit of  state_gpd (millions of pesos) is very different to other variables' units. This, in turn, creates problems when estimating the models
bd_state_gdp <- 
  bd_state_gdp %>% 
  mutate(state_gpd_rescaled = rescale(state_gpd))  


### Create lagged versions of state_gpd_rescaled ----
bd_state_gdp <- 
  bd_state_gdp %>% 
  group_by(state) %>% 
  mutate(state_gpd_rescaled_lag_1 = lag(state_gpd_rescaled),
         state_gpd_rescaled_lag_2 = lag(state_gpd_rescaled, n = 2),
         state_gpd_rescaled_lag_3 = lag(state_gpd_rescaled, n = 3)) %>% 
  ungroup()
  
### Change the name of three states ---- 

# This change is needed to later join this tibble with other  ----
bd_state_gdp <- 
  bd_state_gdp %>% 
  mutate(state = case_when(state == "Coahuila de Zaragoza" ~ "Coahuila",
                            state == "Michoacán de Ocampo" ~ "Michoacán",
                            state == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
                            state == "México" ~ "Estado de México",
                            TRUE ~ state))

