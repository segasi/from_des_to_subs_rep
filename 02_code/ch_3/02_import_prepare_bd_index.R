### Load packages, define setup and vis theme ----
source("02_code/ch_3/00_pacakges_setup_theme.R")

### Import data ----
bd_index <- 
  read_excel("01_data/ch_3/index/02. Aplicación del Índice de Fortaleza de Cuota en la Legislación Federal y Estatal en México (1990-2016) 03.08.2021.xlsx",
             range = "a1:k243") %>% 
  clean_names() %>% 
  rename(ifreg = ifc)

### Filter/eliminate federal values -----
bd_index_state <- 
  bd_index %>% 
  # Eliminar observaciones de normas federales
  filter(!str_detect(entidad, "Federal ")) 

### Eliminate columns that contain subcomponents of the index ----
bd_index_state <- 
  bd_index_state %>% 
  select(-c(tipo_de_eleccion, tamano_de_la_cuota:valvula_de_escape))

### Rename variables ----
bd_index_state <- 
  bd_index_state %>%
  rename(state = entidad,
         law_name = ley,
         law_publication_year = ano_de_publicacion_de_la_ley,
         electoral_rule = principio_de_representacion)


### Clean values of state column -----
bd_index_state <- 
  bd_index_state %>% 
    mutate(state = str_replace(state, " IX| IV| III| II| I| VIII| VII| VII| VI| V| XIV| XIII| XII| XI| X", ""),
           state = ifelse(str_detect(state, "Ciudad de "), "Ciudad de México", state)) %>% 
  # Reorder columns
  select(state, law_name, everything()) 


### Translate values of electoral_rule ----
bd_index_state <- 
  bd_index_state %>% 
  mutate(electoral_rule = case_when(str_detect(electoral_rule, "Rep") ~ "Proportional",
                                    str_detect(electoral_rule, "May") ~ "Majority",
                                    TRUE ~ electoral_rule))

### Create variable n_elections ----

# The original database does not contain a variable that indicates for which elections a particular law was in force This variable, which will be created later and will be called election_yr, is necessary in order to later join the index data to the legislative data. 

# In order to create election_yr we first create n_elections, which measures in how many election was a particular law in force. n_elections will be used in the next step to repeat each observation for as many elections it was in force. After that, we create election_yr.

# When constructing n_elections we consider that a law was in force in the years *after* it was issued, until a new law was approved (if applicable). This implies:
  
# - That if a law was approved in 2009 and there were elections that year, we do not consider that the law was in force for that election, but for subsequent ones. This decision is based on the fact that for Mexican electoral matters the rule usually is that no changes can be made to the law once the process has begun, and this usually happens the year prior to the election.
  
# - That if two norms were issued in 2007 and 2008, and the elections were in 2006 and 2009, the norm that we take into account for 2009 is that of 2008 and the law of 2007 will not be included in the analysis.

bd_index_state <- 
  bd_index_state %>%
  mutate(n_elections = case_when(state == "Aguascalientes" & law_publication_year == 2003 ~ 2,
                                 state == "Aguascalientes" & law_publication_year == 2009 ~ 1,
                                 state == "Aguascalientes" & law_publication_year == 2012 ~ 1,
                                 state == "Aguascalientes" & law_publication_year == 2015 ~ 2,
                                 
                                 state == "Baja California" & law_publication_year == 2006 ~ 1,
                                 state == "Baja California" & law_publication_year == 2008 ~ 2,
                                 state == "Baja California" & law_publication_year == 2015 ~ 2,
                                 
                                 state == "Baja California Sur" & law_publication_year == 2003 ~ 1,
                                 state == "Baja California Sur" & law_publication_year == 2007 ~ 2,
                                 state == "Baja California Sur" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Campeche" & law_publication_year == 2002 ~ 1,
                                 state == "Campeche" & law_publication_year == 2005 ~ 1,
                                 state == "Campeche" & law_publication_year == 2008 ~ 2,
                                 state == "Campeche" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Chiapas" & law_publication_year == 2005 ~ 1,
                                 state == "Chiapas" & law_publication_year == 2008 ~ 1,
                                 state == "Chiapas" & law_publication_year == 2010 ~ 0, 
                                 state == "Chiapas" & law_publication_year == 2011 ~ 1,
                                 state == "Chiapas" & law_publication_year == 2013 ~ 0, 
                                 state == "Chiapas" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Chihuahua" & law_publication_year == 1997 ~ 4,
                                 state == "Chihuahua" & law_publication_year == 2009 ~ 2,
                                 state == "Chihuahua" & law_publication_year == 2014 ~ 0,
                                 state == "Chihuahua" & law_publication_year == 2015 ~ 2,
                                 
                                 state == "Ciudad de México" & law_publication_year == 1999 ~ 2,
                                 state == "Ciudad de México" & law_publication_year == 2003 ~ 0,
                                 state == "Ciudad de México" & law_publication_year == 2005 ~ 1,
                                 state == "Ciudad de México" & law_publication_year == 2008 ~ 1,
                                 state == "Ciudad de México" & law_publication_year == 2011 ~ 1,
                                 state == "Ciudad de México" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Coahuila" & law_publication_year == 2001 ~ 3,
                                 state == "Coahuila" & law_publication_year == 2009 ~ 0,
                                 state == "Coahuila" & law_publication_year == 2010 ~ 2,
                                 state == "Coahuila" & law_publication_year == 2016 ~ 1,
                                 
                                 state == "Colima" & law_publication_year == 1999 ~ 1,
                                 state == "Colima" & law_publication_year == 2002 ~ 1,
                                 state == "Colima" & law_publication_year == 2005 ~ 3,
                                 state == "Colima" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Durango" & law_publication_year == 2000 ~ 3,
                                 state == "Durango" & law_publication_year == 2008 ~ 2,
                                 state == "Durango" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Estado de México" & law_publication_year == 1999 ~ 3,
                                 state == "Estado de México" & law_publication_year == 2008 ~ 2,
                                 state == "Estado de México" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Guanajuato" & law_publication_year == 2002 ~ 4,
                                 state == "Guanajuato" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Guerrero" & law_publication_year == 1998 ~ 2,
                                 state == "Guerrero" & law_publication_year == 2004 ~ 2,
                                 state == "Guerrero" & law_publication_year == 2008 ~ 1,
                                 state == "Guerrero" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Hidalgo" & law_publication_year == 2007 ~ 3,
                                 state == "Hidalgo" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Jalisco" & law_publication_year == 2002 ~ 2,
                                 state == "Jalisco" & law_publication_year == 2008 ~ 2,
                                 state == "Jalisco" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Michoacán" & law_publication_year == 2001 ~ 3,
                                 state == "Michoacán" & law_publication_year == 2012 ~ 0,
                                 state == "Michoacán" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Morelos" & law_publication_year == 2005 ~ 1,
                                 state == "Morelos" & law_publication_year == 2008 ~ 2,
                                 state == "Morelos" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Nayarit" & law_publication_year == 2004 ~ 3,
                                 state == "Nayarit" & law_publication_year == 2013 ~ 1,
                                 state == "Nayarit" & law_publication_year == 2016 ~ 1,
                                 
                                 state == "Nuevo León" & law_publication_year == 2008 ~ 2,
                                 state == "Nuevo León" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Oaxaca" & law_publication_year == 1997 ~ 3,
                                 state == "Oaxaca" & law_publication_year == 2008 ~ 1,
                                 state == "Oaxaca" & law_publication_year == 2012 ~ 1,
                                 state == "Oaxaca" & law_publication_year == 2015 ~ 2,
                                 
                                 state == "Puebla" & law_publication_year == 2000 ~ 4,
                                 state == "Puebla" & law_publication_year == 2012 ~ 0,
                                 state == "Puebla" & law_publication_year == 2013 ~ 1,
                                 state == "Puebla" & law_publication_year == 2015 ~ 1,
                                 
                                 state == "Querétaro" & law_publication_year == 2002 ~ 1,
                                 state == "Querétaro" & law_publication_year == 2005 ~ 1,
                                 state == "Querétaro" & law_publication_year == 2008 ~ 2,
                                 state == "Querétaro" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Quintana Roo" & law_publication_year == 2004 ~ 1,
                                 state == "Quintana Roo" & law_publication_year == 2007 ~ 2,
                                 state == "Quintana Roo" & law_publication_year == 2012 ~ 1,
                                 state == "Quintana Roo" & law_publication_year == 2015 ~ 2,
                                 
                                 state == "San Luis Potosí" & law_publication_year == 1999 ~ 1,
                                 state == "San Luis Potosí" & law_publication_year == 2002 ~ 2,
                                 state == "San Luis Potosí" & law_publication_year == 2008 ~ 1,
                                 state == "San Luis Potosí" & law_publication_year == 2011 ~ 1,
                                 state == "San Luis Potosí" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Sinaloa" & law_publication_year == 1998 ~ 2,
                                 state == "Sinaloa" & law_publication_year == 2006 ~ 2,
                                 state == "Sinaloa" & law_publication_year == 2012 ~ 1,
                                 state == "Sinaloa" & law_publication_year == 2015 ~ 2,
                                 
                                 state == "Sonora" & law_publication_year == 1996 ~ 3,
                                 state == "Sonora" & law_publication_year == 2005 ~ 3,
                                 state == "Sonora" & law_publication_year == 2013 ~ 0,
                                 state == "Sonora" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Tabasco" & law_publication_year == 2002 ~ 2,
                                 state == "Tabasco" & law_publication_year == 2008 ~ 2,
                                 state == "Tabasco" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Tamaulipas" & law_publication_year == 2003 ~ 2,
                                 state == "Tamaulipas" & law_publication_year == 2008 ~ 2,
                                 state == "Tamaulipas" & law_publication_year == 2015 ~ 2,
                                 
                                 state == "Tlaxcala" & law_publication_year == 2004 ~ 1,
                                 state == "Tlaxcala" & law_publication_year == 2008 ~ 2,
                                 state == "Tlaxcala" & law_publication_year == 2015 ~ 2,
                                 
                                 state == "Veracruz" & law_publication_year == 2006 ~ 2,
                                 state == "Veracruz" & law_publication_year == 2012 ~ 1,
                                 state == "Veracruz" & law_publication_year == 2015 ~ 2,
                                 
                                 state == "Yucatán" & law_publication_year == 2003 ~ 1,
                                 state == "Yucatán" & law_publication_year == 2006 ~ 3,
                                 state == "Yucatán" & law_publication_year == 2012 ~ 0,
                                 state == "Yucatán" & law_publication_year == 2014 ~ 2,
                                 
                                 state == "Zacatecas" & law_publication_year == 2003 ~ 1,
                                 state == "Zacatecas" & law_publication_year == 2006 ~ 1,
                                 state == "Zacatecas" & law_publication_year == 2009 ~ 1,
                                 state == "Zacatecas" & law_publication_year == 2012 ~ 1,
                                 state == "Zacatecas" & law_publication_year == 2015 ~ 2))


### Repeat the observation that corresponds to one particular law for as many elections as it was in force ----

# Given that in some cases n_elections is equal to 0 (see previous comments to understand why this can be the case), these observations will be eliminated from the tibble

bd_index_state <- 
  bd_index_state %>% 
  filter(n_elections > 0) %>%
  # Repeat observations
  uncount(weights = n_elections) %>% 
  # Create variable id_election in order to identify the number of elections in which a law was in force
  group_by(state, electoral_rule, law_publication_year) %>%
  mutate(id_election = row_number()) %>%
  ungroup() 


### Create variable election_yr ----

# This variable indicates for which elections an electoral law was in force
bd_index_state <-
  bd_index_state %>% 
  mutate(election_yr = case_when(state == "Aguascalientes" & law_publication_year == 2003 & id_election == 1 ~ 2004,
                                 state == "Aguascalientes" & law_publication_year == 2003 & id_election == 2 ~ 2007,
                                 state == "Aguascalientes" & law_publication_year == 2009 & id_election == 1 ~ 2010,
                                 state == "Aguascalientes" & law_publication_year == 2012 & id_election == 1 ~ 2013,
                                 state == "Aguascalientes" & law_publication_year == 2015 & id_election == 1 ~ 2016,
                                 state == "Aguascalientes" & law_publication_year == 2015 & id_election == 2 ~ 2018,
                                 
                                 state == "Baja California" & law_publication_year == 2006 & id_election == 1 ~ 2007,
                                 state == "Baja California" & law_publication_year == 2008 & id_election == 1 ~ 2010,
                                 state == "Baja California" & law_publication_year == 2008 & id_election == 2 ~ 2013,
                                 state == "Baja California" & law_publication_year == 2015 & id_election == 1 ~ 2016,
                                 state == "Baja California" & law_publication_year == 2015 & id_election == 2 ~ 2019,
                                 
                                 state == "Baja California Sur" & law_publication_year == 2003 & id_election == 1 ~ 2005,
                                 state == "Baja California Sur" & law_publication_year == 2007 & id_election == 1 ~ 2008,
                                 state == "Baja California Sur" & law_publication_year == 2007 & id_election == 2 ~ 2011,
                                 state == "Baja California Sur" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Baja California Sur" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Campeche" & law_publication_year == 2002 & id_election == 1 ~ 2003,
                                 state == "Campeche" & law_publication_year == 2005 & id_election == 1 ~ 2006,
                                 state == "Campeche" & law_publication_year == 2008 & id_election == 1 ~ 2009,
                                 state == "Campeche" & law_publication_year == 2008 & id_election == 2 ~ 2012,
                                 state == "Campeche" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Campeche" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Chiapas" & law_publication_year == 2005 & id_election == 1 ~ 2007,
                                 state == "Chiapas" & law_publication_year == 2008 & id_election == 1 ~ 2010,
                                 state == "Chiapas" & law_publication_year == 2011 & id_election == 1 ~ 2013,
                                 state == "Chiapas" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Chiapas" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Chihuahua" & law_publication_year == 1997 & id_election == 1 ~ 1998,
                                 state == "Chihuahua" & law_publication_year == 1997 & id_election == 2 ~ 2001,
                                 state == "Chihuahua" & law_publication_year == 1997 & id_election == 3 ~ 2004,
                                 state == "Chihuahua" & law_publication_year == 1997 & id_election == 4 ~ 2007,
                                 state == "Chihuahua" & law_publication_year == 2009 & id_election == 1 ~ 2010,
                                 state == "Chihuahua" & law_publication_year == 2009 & id_election == 2 ~ 2013,
                                 state == "Chihuahua" & law_publication_year == 2015 & id_election == 1 ~ 2016,
                                 state == "Chihuahua" & law_publication_year == 2015 & id_election == 2 ~ 2018,
                                 
                                 state == "Ciudad de México" & law_publication_year == 1999 & id_election == 1 ~ 2000,
                                 state == "Ciudad de México" & law_publication_year == 1999 & id_election == 2 ~ 2003,
                                 state == "Ciudad de México" & law_publication_year == 2005 & id_election == 1 ~ 2006,
                                 state == "Ciudad de México" & law_publication_year == 2008 & id_election == 1 ~ 2009,
                                 state == "Ciudad de México" & law_publication_year == 2011 & id_election == 1 ~ 2012,
                                 state == "Ciudad de México" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Ciudad de México" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Coahuila" & law_publication_year == 2001 & id_election == 1 ~ 2003,
                                 state == "Coahuila" & law_publication_year == 2001 & id_election == 2 ~ 2006,
                                 state == "Coahuila" & law_publication_year == 2001 & id_election == 3 ~ 2009,
                                 state == "Coahuila" & law_publication_year == 2010 & id_election == 1 ~ 2012,
                                 state == "Coahuila" & law_publication_year == 2010 & id_election == 2 ~ 2015,
                                 state == "Coahuila" & law_publication_year == 2016 & id_election == 1 ~ 2018,
                                 
                                 state == "Colima" & law_publication_year == 1999 & id_election == 1 ~ 2000,
                                 state == "Colima" & law_publication_year == 2002 & id_election == 1 ~ 2003,
                                 state == "Colima" & law_publication_year == 2005 & id_election == 1 ~ 2006,
                                 state == "Colima" & law_publication_year == 2005 & id_election == 2 ~ 2009,
                                 state == "Colima" & law_publication_year == 2005 & id_election == 3 ~ 2012,
                                 state == "Colima" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Colima" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Durango" & law_publication_year == 2000 & id_election == 1 ~ 2001,
                                 state == "Durango" & law_publication_year == 2000 & id_election == 2 ~ 2004,
                                 state == "Durango" & law_publication_year == 2000 & id_election == 3 ~ 2007,
                                 state == "Durango" & law_publication_year == 2008 & id_election == 1 ~ 2010,
                                 state == "Durango" & law_publication_year == 2008 & id_election == 2 ~ 2013,
                                 state == "Durango" & law_publication_year == 2014 & id_election == 1 ~ 2016,
                                 state == "Durango" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Estado de México" & law_publication_year == 1999 & id_election == 1 ~ 2000,
                                 state == "Estado de México" & law_publication_year == 1999 & id_election == 2 ~ 2003,
                                 state == "Estado de México" & law_publication_year == 1999 & id_election == 3 ~ 2006,
                                 state == "Estado de México" & law_publication_year == 2008 & id_election == 1 ~ 2009,
                                 state == "Estado de México" & law_publication_year == 2008 & id_election == 2 ~ 2012,
                                 state == "Estado de México" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Estado de México" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Guanajuato" & law_publication_year == 2002 & id_election == 1 ~ 2003,
                                 state == "Guanajuato" & law_publication_year == 2002 & id_election == 2 ~ 2006,
                                 state == "Guanajuato" & law_publication_year == 2002 & id_election == 3 ~ 2009,
                                 state == "Guanajuato" & law_publication_year == 2002 & id_election == 4 ~ 2012,
                                 state == "Guanajuato" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Guanajuato" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Guerrero" & law_publication_year == 1998 & id_election == 1 ~ 1999,
                                 state == "Guerrero" & law_publication_year == 1998 & id_election == 2 ~ 2002,
                                 state == "Guerrero" & law_publication_year == 2004 & id_election == 1 ~ 2005,
                                 state == "Guerrero" & law_publication_year == 2004 & id_election == 2 ~ 2008,
                                 state == "Guerrero" & law_publication_year == 2008 & id_election == 1 ~ 2012,
                                 state == "Guerrero" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Guerrero" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Hidalgo" & law_publication_year == 2007 & id_election == 1 ~ 2008,
                                 state == "Hidalgo" & law_publication_year == 2007 & id_election == 2 ~ 2010,
                                 state == "Hidalgo" & law_publication_year == 2007 & id_election == 3 ~ 2013,
                                 state == "Hidalgo" & law_publication_year == 2014 & id_election == 1 ~ 2016,
                                 state == "Hidalgo" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Jalisco" & law_publication_year == 2002 & id_election == 1 ~ 2003,
                                 state == "Jalisco" & law_publication_year == 2002 & id_election == 2 ~ 2006,
                                 state == "Jalisco" & law_publication_year == 2008 & id_election == 1 ~ 2009,
                                 state == "Jalisco" & law_publication_year == 2008 & id_election == 2 ~ 2012,
                                 state == "Jalisco" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Jalisco" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Michoacán" & law_publication_year == 2001 & id_election == 1 ~ 2004,
                                 state == "Michoacán" & law_publication_year == 2001 & id_election == 2 ~ 2008,
                                 state == "Michoacán" & law_publication_year == 2001 & id_election == 3 ~ 2011,
                                 state == "Michoacán" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Michoacán" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Morelos" & law_publication_year == 2005 & id_election == 1 ~ 2006, 
                                 state == "Morelos" & law_publication_year == 2008 & id_election == 1 ~ 2009,
                                 state == "Morelos" & law_publication_year == 2008 & id_election == 2 ~ 2012,
                                 state == "Morelos" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Morelos" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Nayarit" & law_publication_year == 2004 & id_election == 1 ~ 2005,
                                 state == "Nayarit" & law_publication_year == 2004 & id_election == 2 ~ 2008,
                                 state == "Nayarit" & law_publication_year == 2004 & id_election == 3 ~ 2011,
                                 state == "Nayarit" & law_publication_year == 2013 & id_election == 1 ~ 2014,
                                 state == "Nayarit" & law_publication_year == 2016 & id_election == 1 ~ 2017,
                                 
                                 state == "Nuevo León" & law_publication_year == 2008 & id_election == 1 ~ 2009,
                                 state == "Nuevo León" & law_publication_year == 2008 & id_election == 2 ~ 2012,
                                 state == "Nuevo León" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Nuevo León" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Oaxaca" & law_publication_year == 1997 & id_election == 1 ~ 2001,
                                 state == "Oaxaca" & law_publication_year == 1997 & id_election == 2 ~ 2004,
                                 state == "Oaxaca" & law_publication_year == 1997 & id_election == 3 ~ 2007,
                                 state == "Oaxaca" & law_publication_year == 2008 & id_election == 1 ~ 2010,
                                 state == "Oaxaca" & law_publication_year == 2012 & id_election == 1 ~ 2013,
                                 state == "Oaxaca" & law_publication_year == 2015 & id_election == 1 ~ 2016,
                                 state == "Oaxaca" & law_publication_year == 2015 & id_election == 2 ~ 2018,
                                 
                                 state == "Puebla" & law_publication_year == 2000 & id_election == 1 ~ 2001,
                                 state == "Puebla" & law_publication_year == 2000 & id_election == 2 ~ 2004,
                                 state == "Puebla" & law_publication_year == 2000 & id_election == 3 ~ 2007,
                                 state == "Puebla" & law_publication_year == 2000 & id_election == 4 ~ 2010,
                                 state == "Puebla" & law_publication_year == 2013 & id_election == 1 ~ 2014,
                                 state == "Puebla" & law_publication_year == 2015 & id_election == 1 ~ 2018,
                                 
                                 state == "Querétaro" & law_publication_year == 2002 & id_election == 1 ~ 2003,
                                 state == "Querétaro" & law_publication_year == 2005 & id_election == 1 ~ 2006,
                                 state == "Querétaro" & law_publication_year == 2008 & id_election == 1 ~ 2009,
                                 state == "Querétaro" & law_publication_year == 2008 & id_election == 2 ~ 2012,
                                 state == "Querétaro" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Querétaro" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Quintana Roo" & law_publication_year == 2004 & id_election == 1 ~ 2005,
                                 state == "Quintana Roo" & law_publication_year == 2007 & id_election == 1 ~ 2008,
                                 state == "Quintana Roo" & law_publication_year == 2007 & id_election == 2 ~ 2011,
                                 state == "Quintana Roo" & law_publication_year == 2012 & id_election == 1 ~ 2013,
                                 state == "Quintana Roo" & law_publication_year == 2015 & id_election == 1 ~ 2016,
                                 state == "Quintana Roo" & law_publication_year == 2015 & id_election == 2 ~ 2019,
                                 
                                 state == "San Luis Potosí" & law_publication_year == 1999 & id_election == 1 ~ 2000,
                                 state == "San Luis Potosí" & law_publication_year == 2002 & id_election == 1 ~ 2003,
                                 state == "San Luis Potosí" & law_publication_year == 2002 & id_election == 2 ~ 2006,
                                 state == "San Luis Potosí" & law_publication_year == 2008 & id_election == 1 ~ 2009,
                                 state == "San Luis Potosí" & law_publication_year == 2011 & id_election == 1 ~ 2012,
                                 state == "San Luis Potosí" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "San Luis Potosí" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Sinaloa" & law_publication_year == 1998 & id_election == 1 ~ 2001,
                                 state == "Sinaloa" & law_publication_year == 1998 & id_election == 2 ~ 2004,
                                 state == "Sinaloa" & law_publication_year == 2006 & id_election == 1 ~ 2007,
                                 state == "Sinaloa" & law_publication_year == 2006 & id_election == 2 ~ 2010,
                                 state == "Sinaloa" & law_publication_year == 2012 & id_election == 1 ~ 2013,
                                 state == "Sinaloa" & law_publication_year == 2015 & id_election == 1 ~ 2016,
                                 state == "Sinaloa" & law_publication_year == 2015 & id_election == 2 ~ 2018,
                                 
                                 state == "Sonora" & law_publication_year == 1996 & id_election == 1 ~ 1997,
                                 state == "Sonora" & law_publication_year == 1996 & id_election == 2 ~ 2000,
                                 state == "Sonora" & law_publication_year == 1996 & id_election == 3 ~ 2003,
                                 state == "Sonora" & law_publication_year == 2005 & id_election == 1 ~ 2006,
                                 state == "Sonora" & law_publication_year == 2005 & id_election == 2 ~ 2009,
                                 state == "Sonora" & law_publication_year == 2005 & id_election == 3 ~ 2012,
                                 state == "Sonora" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Sonora" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Tabasco" & law_publication_year == 2002 & id_election == 1 ~ 2004,
                                 state == "Tabasco" & law_publication_year == 2002 & id_election == 2 ~ 2007,
                                 state == "Tabasco" & law_publication_year == 2008 & id_election == 1 ~ 2010,
                                 state == "Tabasco" & law_publication_year == 2008 & id_election == 2 ~ 2013,
                                 state == "Tabasco" & law_publication_year == 2014 & id_election == 1 ~ 2016,
                                 state == "Tabasco" & law_publication_year == 2014 & id_election == 2 ~ 2019,
                                 
                                 state == "Tamaulipas" & law_publication_year == 2003 & id_election == 1 ~ 2005,
                                 state == "Tamaulipas" & law_publication_year == 2003 & id_election == 2 ~ 2008,
                                 state == "Tamaulipas" & law_publication_year == 2008 & id_election == 1 ~ 2011,
                                 state == "Tamaulipas" & law_publication_year == 2008 & id_election == 2 ~ 2013,
                                 state == "Tamaulipas" & law_publication_year == 2015 & id_election == 1 ~ 2016,
                                 state == "Tamaulipas" & law_publication_year == 2015 & id_election == 2 ~ 2019,
                                 
                                 state == "Tlaxcala" & law_publication_year == 2004 & id_election == 1 ~ 2007,
                                 state == "Tlaxcala" & law_publication_year == 2008 & id_election == 1 ~ 2010,
                                 state == "Tlaxcala" & law_publication_year == 2008 & id_election == 2 ~ 2013,
                                 state == "Tlaxcala" & law_publication_year == 2015 & id_election == 1 ~ 2016,
                                 state == "Tlaxcala" & law_publication_year == 2015 & id_election == 2 ~ 2018,
                                 
                                 state == "Veracruz" & law_publication_year == 2006 & id_election == 1 ~ 2007,
                                 state == "Veracruz" & law_publication_year == 2006 & id_election == 2 ~ 2010,
                                 state == "Veracruz" & law_publication_year == 2012 & id_election == 1 ~ 2013,
                                 state == "Veracruz" & law_publication_year == 2015 & id_election == 1 ~ 2016,
                                 state == "Veracruz" & law_publication_year == 2015 & id_election == 2 ~ 2018,
                                 
                                 state == "Yucatán" & law_publication_year == 2003 & id_election == 1 ~ 2004,
                                 state == "Yucatán" & law_publication_year == 2006 & id_election == 1 ~ 2007,
                                 state == "Yucatán" & law_publication_year == 2006 & id_election == 2 ~ 2010,
                                 state == "Yucatán" & law_publication_year == 2006 & id_election == 3 ~ 2012,
                                 state == "Yucatán" & law_publication_year == 2014 & id_election == 1 ~ 2015,
                                 state == "Yucatán" & law_publication_year == 2014 & id_election == 2 ~ 2018,
                                 
                                 state == "Zacatecas" & law_publication_year == 2003 & id_election == 1 ~ 2004,
                                 state == "Zacatecas" & law_publication_year == 2006 & id_election == 1 ~ 2007,
                                 state == "Zacatecas" & law_publication_year == 2009 & id_election == 1 ~ 2010,
                                 state == "Zacatecas" & law_publication_year == 2012 & id_election == 1 ~ 2013,
                                 state == "Zacatecas" & law_publication_year == 2015 & id_election == 1 ~ 2016,
                                 state == "Zacatecas" & law_publication_year == 2015 & id_election == 2 ~ 2018)) %>% 
  select(state:law_publication_year, election_yr, everything())
