### Load packages, define setup and vis theme ----
source("02_code/ch_3/00_pacakges_setup_theme.R")

### Import data ----

# Population estimates calculated by CONAPO 

# Source: 

# Población a mitad de año (mid-year population)

# Población a mitad de año. For "República Mexicana" the period covers from 1950 to 2050; for the states it ranges from 1970 to 2050.

# http://www.conapo.gob.mx/work/models/CONAPO/Datos_Abiertos/Proyecciones2018/pob_mit_proyecciones.csv

bd_pop <- 
  read_delim("01_data/ch_3/conapo/pob_mit_proyecciones.csv", "," , locale = locale(encoding = "latin1")) %>% 
  clean_names() 

### Rename some variables ----
bd_pop <- 
  bd_pop%>% 
  rename(year = ano,
         state = entidad,
         population = poblacion)

### Calculate population by state and year ----
bd_pop <- 
  bd_pop %>% 
  # Eliminate country observations
  filter(state != "República Mexicana") %>% 
  # Change values for Estado de México
  mutate(state = ifelse(state == "México", "Estado de México", state)) %>% 
  group_by(year, state) %>% 
  summarise(population = sum(population)) %>% 
  ungroup() 
