### Load packages, define setup and vis theme ----
source("02_code/ch_3/00_pacakges_setup_theme.R")

### Average years of education ----

## Data for 2000, 2010, 2015 and 2020 ----

# Import data ----

# Source: https://www.inegi.org.mx/app/tabulados/interactivos/?pxq=Educacion_Educacion_05_a74f77b9-42b8-40fc-a6dd-152f3a4e91f4
bd_edu_00_20 <- 
  read_excel("01_data/ch_3/inegi/education/Educacion_05.xlsx", 
             range = "a5:k38") 

# Select variables and filter observations ----
bd_edu_00_20 <- 
  bd_edu_00_20 %>% 
  select(`Entidad federativa`, contains("Total")) %>% 
  filter(!str_detect(`Entidad federativa`, "Unidos")) 

# Rename variables ----
bd_edu_00_20 <- 
  bd_edu_00_20 %>% 
  rename(state = `Entidad federativa` ,
         `2000` = Total...2,
         `2010` = Total...5,
         `2015` = Total...8,
         `2020` = Total...11) 

# Change data structure from wide to long ----
bd_edu_00_20 <- 
  bd_edu_00_20 %>% 
  pivot_longer(-state, 
               names_to = "year", 
               values_to = "avg_yr_school")


## Data for 2005 ----

# Import data ----

bd_edu_05 <-  
  read_excel("01_data/ch_3/inegi/education/educacion_00.xlsx") 

# Filter observations and select variables  ----
bd_edu_05 <- 
  bd_edu_05 %>%
  filter(desc_municipio == "Estatal",
         id_indicador == 1005000038) %>% 
  select(state = desc_entidad, `2005`) 

# Change data structure from wide to long ----
bd_edu_05 <- 
  bd_edu_05 %>% 
  pivot_longer(-state, 
               names_to = "year", 
               values_to = "avg_yr_school") %>% 
  mutate(avg_yr_school = as.numeric(avg_yr_school))

## Join tibbles ----
bd_edu <- 
  bd_edu_00_20 %>% 
  bind_rows(bd_edu_05) 

## Homogenize state names ---- 
bd_edu <- 
  bd_edu %>% 
  mutate(state = case_when(state == "Coahuila de Zaragoza" ~ "Coahuila",
                           state == "Michoacán de Ocampo" ~ "Michoacán",
                           state == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
                           state == "México" ~ "Estado de México",
                           TRUE ~ state), 
         year = as.numeric(year))


## Rearrange rows ----
bd_edu <- 
  bd_edu %>% 
  arrange(state, year)


## Verify visually ----
bd_edu %>% 
  ggplot(aes(x = year, y = avg_yr_school, group = state)) +
  geom_line() +
  facet_wrap(~ state, ncol = 8)

### Percentage of the population 15 yrs and older with higher education studies ----

## Data for 2000 a 2015 ----

# Source: https://www.inegi.org.mx/app/tabulados/interactivos/?pxq=Educacion_Educacion_04_9209e1cf-72e7-430b-ac83-9c96d3166750

# Importa data ----
bd_high_edu <- 
  read_excel("01_data/ch_3/inegi/education/Educacion_04.xlsx", range = "a5:f37") %>% 
  clean_names() 

# Select and rename variables ----
bd_high_edu <-  
  bd_high_edu %>% 
  select(state = entidad_federativa, `2000` = total_3, `2005` = total_4, `2010` = total_5, `2015` = total_6) 

# Replicate data of 2015 for 2020
bd_high_edu <-  
  bd_high_edu %>% 
  mutate(`2020` = `2015`)

# Change data structure from wide to long ----
bd_high_edu <-  
  bd_high_edu %>% 
  pivot_longer(-state, names_to = "year", values_to = "percent_pop_high_edu") %>% 
  mutate(year = as.numeric(year))

## Homogenize state names ---- 
bd_high_edu <-  
  bd_high_edu %>% 
  mutate(state = str_trim(state),
         state = case_when(str_detect(state, "Coah") ~ "Coahuila",
                           state == "México" ~ "Estado de México",
                           str_detect(state, "Mich") ~ "Michoacán",
                           str_detect(state, "Veracruz") ~ "Veracruz", 
                           TRUE ~ state))



### Percentage urban population ----

# Import data on population living in localities with less or more than 2,500 inhabitants ----

# Sources: 

# Data from census (1990, 2000, 2010, 2020) and population counts. The data from the 2015 intercensus poll (encuesta intercensal) will be processed later in this script  

# According to INEGI, Mexico's Census Bureau, a location with more than 2,500 inhabitants is considered urban, and rural otherwise  (http://cuentame.inegi.org.mx/poblacion/rur_urb.aspx?tema_P; https://www.inegi.org.mx/contenidos/programas/enigh/nc/2020/doc/enigh2020_ns_presentacion_resultados.pdf)

# Links: 

# Data for the period 1990 a 2010: https://www.inegi.org.mx/programas/ccpv/cpvsh/

# Data for 2015: https://www.inegi.org.mx/programas/intercensal/2015/

# Data for 2020: https://www.inegi.org.mx/sistemas/Olap/Proyectos/bd/censos/cpv2020/pt.asp

# Note: the files generated by INEGI's platform have a .xls forma, which can't be opened with {readxl}. Thus, we save them as .xlsx. The problems with the accents come from the original file.

# 1990
bd_pop_1990 <- 
  read_excel("01_data/ch_3/inegi/urbanization/INEGI_Exporta_20210122162417_poblacion_tamanio_comunidad_1990.xlsx")

# 1995
bd_pop_1995 <- 
  read_excel("01_data/ch_3/inegi/urbanization/INEGI_Exporta_20210122162417_poblacion_tamanio_comunidad_1995.xlsx")

# 2000
bd_pop_2000 <- 
  read_excel("01_data/ch_3/inegi/urbanization/INEGI_Exporta_20210122162417_poblacion_tamanio_comunidad_2000.xlsx")

# 2005
bd_pop_2005 <- 
  read_excel("01_data/ch_3/inegi/urbanization/INEGI_Exporta_20210122162417_poblacion_tamanio_comunidad_2005.xlsx")

# 2010
bd_pop_2010 <- 
  read_excel("01_data/ch_3/inegi/urbanization/INEGI_Exporta_20210122162417_poblacion_tamanio_comunidad_2010.xlsx")

# 2020
bd_pop_2020 <- 
  read_excel("01_data/ch_3/inegi/urbanization/INEGI_exporta_29_7_2021_12_45_35_poblacion_tamanio_comunidad_2020.xlsx")


# Clean and homogenize data from census and population counts ----

# Data for 1990, 1995, 2005, 2010 and 2020
clean_bd_pop <- function(bd) {
  bd <- 
    bd %>% 
    # Select and rename variables
    select(state = `...2`, total = `...3`, less_2.5K = `...4`, more_2.5K = `...5`) %>% 
    # Eliminate rows without values for state
    filter(!is.na(state),
           # Eliminate rows with totals 
           state != "Total",
           # Eliminate country data 
           !str_detect(state, "Unidos")) %>% 
    # Clean and homogenize state names
    mutate(state = str_trim(state),
           state = case_when(str_detect(state, "Coah") ~ "Coahuila",
                             str_detect(state, "M鸩co") ~ "Estado de México",
                             state == "México" ~ "Estado de México",
                             str_detect(state, "Distrito") ~ "Ciudad de México",
                             
                             str_detect(state, "Mich") ~ "Michoacán",
                             str_detect(state, "Nuevo L") ~ "Nuevo León",
                             str_detect(state, "Quer") ~ "Querétaro",
                             str_detect(state, "San") ~ "San Luis Potosí",
                             str_detect(state, "Veracruz") ~ "Veracruz",
                             str_detect(state, "Yuc") ~ "Yucatán",
                             TRUE ~ state),
           
           total = str_replace_all(total, ",", ""),
           less_2.5K = str_replace_all(less_2.5K, ",", ""),
           more_2.5K = str_replace_all(more_2.5K, ",", ""),
           total = as.numeric(total),
           less_2.5K = as.numeric(less_2.5K),
           more_2.5K = as.numeric(more_2.5K),
           percent_urban_pop = more_2.5K/total*100) %>% 
    select(state, percent_urban_pop)
}


bd_pop_1990 <- clean_bd_pop(bd_pop_1990) %>% mutate(year = 1990)
bd_pop_1995 <- clean_bd_pop(bd_pop_1995) %>% mutate(year = 1995)
bd_pop_2005 <- clean_bd_pop(bd_pop_2005) %>% mutate(year = 2005)
bd_pop_2010 <- clean_bd_pop(bd_pop_2010) %>% mutate(year = 2010)
bd_pop_2020 <- clean_bd_pop(bd_pop_2020) %>% mutate(year = 2020)


# 2000 census data

# This data must be cleaned independently because the file's structure is different from the already processed

bd_pop_2000 <- 
  bd_pop_2000 %>% 
  # Select and rename variables
  select(state = `...2`, 
         total = `...3`, 
         less_2.5K = `...4`, 
         from_2.5_to_14.5K = `...5`,
         from_15_to_99.9K = `...6`,
         more_99.9K = `...7`) %>% 
  # Eliminate rows without values for state
  filter(!is.na(state),
         # Eliminate rows with totals 
         state != "Total",
         # Eliminate country data 
         !str_detect(state, "Unidos")) %>% 
  # Clean and homogenize state names
  mutate(state = str_trim(state),
         state = case_when(str_detect(state, "Coah") ~ "Coahuila",
                           str_detect(state, "M鸩co") ~ "Estado de México",
                           state == "México" ~ "Estado de México",
                           str_detect(state, "Distrito") ~ "Ciudad de México",
                           
                           str_detect(state, "Mich") ~ "Michoacán",
                           str_detect(state, "Nuevo L") ~ "Nuevo León",
                           str_detect(state, "Quer") ~ "Querétaro",
                           str_detect(state, "San") ~ "San Luis Potosí",
                           str_detect(state, "Veracruz") ~ "Veracruz",
                           str_detect(state, "Yuc") ~ "Yucatán",
                           TRUE ~ state),
         total = as.numeric(total),
         less_2.5K = as.numeric(less_2.5K),
         percent_urban_pop = (total - less_2.5K)/total*100, 
         year = 2000) %>% 
  select(state, percent_urban_pop, year)



# Import files of the 2015 intercensus poll (encuesta intercensal) on population living in localities with less or more than 2,500 inhabitants ----

# Create list with files
pop_files <- 
  list.files(path = "01_data/ch_3/inegi/urbanization/encuesta_intercensal_2015/", pattern = "01_poblacion")

# Join all tibbles in one file for the 2015 data
bd_pop_2015 <- 
  pop_files %>% 
  map(function(x) {
    read_excel(paste0("01_data/ch_3/inegi/urbanization/encuesta_intercensal_2015/", x), sheet = "01", skip = 7, col_names = F) %>% 
      clean_names() 
  })  %>% 
  # Join all tibbles
  reduce(rbind) 


# Clean and homogenize data for the 2015 intercensus poll ----

# Rename, filter and select columns; edit values of state names 
bd_pop_2015 <- 
  bd_pop_2015 %>% 
  # Rename variables
  rename(state = x1, 
         loc_size = x2, 
         age_gpo = x3,
         estimator = x4, 
         pop_tot = x5, 
         pop_hombres = x6,
         pop_mujeres = x7) %>% 
  # Filter rows
  filter(loc_size %in% c("Total", "Menos de 2 500 habitantes"), 
         age_gpo == "Total",
         estimator == "Valor") %>%
  # Select columns
  select(state, loc_size, pop_tot)

# Homogenize state names ----
bd_pop_2015 <- 
  bd_pop_2015 %>% 
  # Create variable year, state varibale without the state geo code and edit state names
  mutate(year = 2015,
         state = str_sub(state, start = 4, end = 150),
         state = str_trim(state),
         state = case_when(state == "Coahuila de Zaragoza" ~ "Coahuila",
                           state == "Michoacán de Ocampo" ~ "Michoacán",
                           state == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
                           state == "México" ~ "Estado de México",
                           TRUE ~ state)) 


# Change data structure from wide to long ----
bd_pop_2015 <- 
  bd_pop_2015 %>% 
  pivot_wider(names_from = "loc_size",
              values_from = "pop_tot") 

# Create percent_urban_pop, share of urban population ----
bd_pop_2015 <- 
  bd_pop_2015 %>% 
  mutate(percent_urban_pop = 100 - (`Menos de 2 500 habitantes`/Total*100)) %>% 
  # Select columns
  select(state, percent_urban_pop, year) 


# Join data on population living in localities with less or more than 2,500 inhabitants ---- 
bd_urban_pop <- 
  bind_rows(bd_pop_1990, bd_pop_1995, bd_pop_2000, bd_pop_2005, bd_pop_2010, bd_pop_2015, bd_pop_2020)

# Verify visualy
bd_urban_pop %>% 
  ggplot(aes(x = year, y = percent_urban_pop)) +
  geom_line() +
  facet_wrap(~ state, ncol = 8)


### Create tibbel bd_year_info_inegi with column year_info_inegi ---- 

# Column year_info_inegi registers the year in which INEGI collected the data for the three diferent variables previously created: percent_urban_pop, avg_yr_school and percent_pop_high_edu.

# Given that these three variables are measured every five years and the final database will have an observation for every year in which each state legislation starts (this could happen every two, three, four or five years, depending on the case), in order to join the variable we adopted the following criteria: to join the data collected in one particular year (e.g., 1990) to that year as well as the two previous and two posterior years (1988, 1989, 1991 y 1992). The idea is to have the most valid measurement of a variable. The only exception is for the observations of 1987, because INEGI did not do a population count in 1985. Thus, for this year we use the data of 1990 census.
bd_year_info_inegi <- 
  tibble(year = 1987:2020) %>% 
  mutate(year_info_inegi = ifelse(year %in% c(1990, 1995, 2000, 2005, 2010, 2015, 2020), year, NA)) %>%
  fill(year_info_inegi, .direction = "up") %>% 
  fill(year_info_inegi, .direction = "down") %>% 
  mutate(foo = year - year_info_inegi,
         year_info_inegi = ifelse(year - year_info_inegi >= -2, year_info_inegi, year_info_inegi - 5),
         year_info_inegi = ifelse(year_info_inegi == 1985, 1990, year_info_inegi)) %>% 
  select(-foo) 

### Join year_info_inegi to bd_urban_pop y bd_high_edu ----
bd_urban_pop <- 
  bd_year_info_inegi %>% 
  left_join(bd_urban_pop, by = c("year_info_inegi" = "year")) %>% 
  select(state, everything()) 

bd_edu <- 
  bd_year_info_inegi %>% 
  left_join(bd_edu, by = c("year_info_inegi" = "year")) %>% 
  select(state, everything()) %>% 
  filter(!is.na(state)) 

bd_high_edu <-  
  bd_year_info_inegi %>% 
  left_join(bd_high_edu, by = c("year_info_inegi" = "year")) %>% 
  select(state, everything()) %>% 
  filter(!is.na(state)) 

### Check state names ----
bd_urban_pop %>% distinct(state) %>% print(n = Inf)
bd_edu %>% distinct(state) %>% print(n = Inf)
bd_high_edu %>% distinct(state) %>% print(n = Inf)


### Vis ----

# Average years of education
bd_edu %>% 
  ggplot(aes(x = year_info_inegi, y = avg_yr_school)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ state, ncol = 8) + 
  labs(title = "Average years of education") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# % population with at least one year of higher education
bd_high_edu %>% 
  ggplot(aes(x = year_info_inegi, y = percent_pop_high_edu)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ state, ncol = 8) + 
  labs(title = "% population with at least one year of higher education") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


# % urban population
bd_urban_pop %>% 
  ggplot(aes(x = year_info_inegi, y = percent_urban_pop)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  facet_wrap(~ state, ncol = 8) + 
  labs(title = "% urban population") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

