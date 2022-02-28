### Load packages, define setup and vis theme ----
source("02_code/ch_3/00_pacakges_setup_theme.R")

### Import state data ----

# This data was obtained from each statet electoral authority and it was used for this paper: http://ojs.uc.cl/index.php/rcp/article/view/33855.

# The period covered in this data sets goes from the early 1990s (it varies by state) until 2019. However, while preparing the data for the book we found out some inconsistencies for the last two legislatures in some states. For this reason, we wont use the data from this source for the last two legislatures in each state. Instead, we will use a new database that will be loaded and processed later in the script.

ags <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Aguascalientes.xlsx",
             sheet = "Lista Histórica Aguascalientes", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

bc <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Baja-California.xlsx",
             sheet = "Lista Histórico BC", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)


bcs <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Baja-California-Sur.xlsx",
             sheet = "Lista Histórico BCS", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion) %>% 
  mutate(estado = "Baja California Sur") %>% 
  # This observation is deleted because Tuchman quitted the PAN before the election and a new election was later held for district 2. Source: https://es.wikipedia.org/wiki/Anexo:VII_Legislatura_del_Congreso_del_Estado_de_Baja_California_Sur
  filter(!str_detect(apellido, "Tuchman"))

camp <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Campeche.xlsx",
             sheet = "Lista Histórico Campeche", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

chis <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Chiapas.xlsx",
             sheet = "Lista Histórico Chiapas", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

chih <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Chihuahua.xlsx",
             sheet = "Lista Histórico Chihuahua", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

cdmx <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Ciudad de México.xlsx",
             sheet = "Lista Histórico CDMX", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

coah <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Coahuila.xlsx",
             sheet = "Lista Histórica Coahuila", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

col <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Colima.xlsx",
             sheet = "Lista Histórico Colima", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

dgo <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Durango.xlsx",
             sheet = "Lista Histórico Durango", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

mex <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Estado-de-Mexico.xlsx",
             sheet = "Listado Histórico EdoMex", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion) %>% 
  mutate(estado = "Estado de México")

gto <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Guanajuato.xlsx",
             sheet = "Lista Histórico Guanajuato", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

gro <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Guerrero.xlsx",
             sheet = "Lista Histórico Guerrero", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

hgo <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Hidalgo.xlsx",
             sheet = "Lista Histórico Hidalgo", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

jal <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Jalisco.xlsx",
             sheet = "Lista Histórica Jalisco", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion) %>% 
  rename(distrito_electoral = distrito_electoral_local)

mich <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Michoacán.xlsx",
             sheet = "Lista Histórico Michoacán", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

mor <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Morelos.xlsx",
             sheet = "Lista Histórico Morelos", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

nay <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Nayarit.xlsx",
             sheet = "Lista Histórico Nayarit", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

nl <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Nuevo-León.xlsx",
            skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

oax <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Oaxaca.xlsx",
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

pue <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Puebla.xlsx",
             sheet = "Lista Histórico Puebla", 
             range = "a2:j494") %>% 
  clean_names() %>% 
  select(-circunscripcion)

qro <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Querétaro.xlsx",
             sheet = "Lista Histórico Querétaro", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

qroo <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Quintana-Roo.xlsx",
             sheet = "Lista Histórico Quintana Roo", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

slp <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-San-Luis-Potosi.xlsx",
             sheet = "Lista Histórico SLP", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

sin <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Sinaloa.xlsx",
             sheet = "Lista Histórico Sinaloa", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

son <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Sonora.xlsx",
             sheet = "Lista Histórico Sonora", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

tab <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Tabasco.xlsx",
             sheet = "Lista Histórico Tabasco", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

tamps <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Tamaulipas.xlsx",
             sheet = "Lista Histórico Tamaulipas", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

tlax <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Tlaxcala.xlsx",
             sheet = "Lista Histórico Tlaxcala", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

ver <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Veracruz.xlsx",
             sheet = "Lista Histórico Veracruz", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

yuc <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Yucatán.xlsx", 
             sheet = "Listá Histórico Yucatán", 
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)

zac <- 
  read_excel("01_data/ch_3/electoral_results/electas/Base-Electas-Zacatecas.xlsx", 
             sheet = "Lista Histórico Zacatecas",
             skip = 1) %>% 
  clean_names() %>% 
  select(-circunscripcion)


## Join all tibbles loaded so far and create a new one called bd_leg_old ----
bd_leg_old <- 
  bind_rows(ags, bc, bcs, camp, chis, chih, cdmx, coah,
            col, dgo, mex, gto, gro, hgo, jal, mich,
            mor, nay, nl, oax, pue, qro, qroo, slp,
            sin, son, tab, tamps, tlax, ver, yuc, zac)

## Rename variables ----
bd_leg_old <- 
  bd_leg_old %>%
  rename(state = estado,
         period = periodo,
         last_name = apellido,
         name = nombre,
         gender = sexo,
         party = partido_politico,
         propietary_alternate = propietario_o_suplente,
         electoral_rule = principio_de_representacion,
         electoral_district = distrito_electoral,
         electoral_district_loc = distrito_electoral_local)

## Homogenize values for variables gender, electoral_rule and propietary_alternate ----
bd_leg_old <- 
  bd_leg_old %>% 
  mutate(gender = str_to_sentence(gender), 
         gender = case_when(gender == "Hombre" ~ "Male",
                            gender == "H" ~ "Male",
                            gender == "Masculino" ~ "Male",
                            gender == "Mujer" ~ "Female",
                            gender == "M" ~ "Female",
                            gender == "Femenino" ~ "Female",
                            TRUE ~ gender),
         electoral_rule = str_to_title(electoral_rule),
         electoral_rule = case_when(str_detect(electoral_rule, "Rep") ~ "Proportional",
                                    str_detect(electoral_rule, "Rp") ~ "Proportional",
                                    str_detect(electoral_rule, "Pluri") ~ "Proportional",
                                    str_detect(electoral_rule, "May") ~ "Mayority",
                                    str_detect(electoral_rule, "Mr") ~ "Mayority",
                                    TRUE ~ electoral_rule),
         propietary_alternate = str_to_title(propietary_alternate),
         propietary_alternate = ifelse(str_detect(propietary_alternate, "Pr"),  "Propietary", "Alternate")) 


## Create tibble that only has data for proprietary legislators ---- 
bd_leg_old_propietary <- 
  bd_leg_old %>% 
  filter(propietary_alternate == "Propietary")

## Calculate number and proportion of legislators by state, gender and legislature ----

# Both principles ----
bd_leg_old_propietary_all <- 
  bd_leg_old_propietary %>% 
  # Calculate number and proportion of legislators by state, gender and legislature, as well as total number of seats by legislature
  group_by(state, period, .drop = FALSE) %>% 
  summarise(n_female_legislators = sum(gender == "Female"), 
            n_male_legislators = sum(gender == "Male"),
            total_legislators = n(),
            prop_female_legislators = n_female_legislators/total_legislators,
            prop_male_legislators = n_male_legislators/total_legislators) %>% 
  ungroup() %>% 
  # Create variable to identify the year when each legislature started
  mutate(start_yr = as.numeric(str_sub(period, start = 1, end = 4))) %>% 
  select(state, period, start_yr, everything())


# Majority rule ----
bd_leg_old_propietary_mr <- 
  bd_leg_old_propietary %>% 
  # Filter in order to only keep the observations of legislators elected by majority
  filter(electoral_rule == "Mayority") %>% 
  # Calculate number and proportion of legislators by state, gender and legislature, as well as total number of seats by legislature
  group_by(state, period, .drop = FALSE) %>% 
  summarise(n_female_legislators = sum(gender == "Female"), 
            n_male_legislators = sum(gender == "Male"),
            total_legislators = n(),
            prop_female_legislators = n_female_legislators/total_legislators,
            prop_male_legislators = n_male_legislators/total_legislators) %>% 
  ungroup() %>% 
  # Create variable to identify the year when each legislature started
  mutate(electoral_rule = "Mayority",
         start_yr = as.numeric(str_sub(period, start = 1, end = 4))) %>% 
  select(state, period, start_yr, everything())

# Proportional representation ----
bd_leg_old_propietary_pr <- 
  bd_leg_old_propietary %>% 
  # Filter in order to only keep the observations of legislators elected by proportional representation
  filter(electoral_rule == "Proportional") %>% 
  # Calculate number and proportion of legislators by state, gender and legislature, as well as total number of seats by legislature
  group_by(state, period, .drop = FALSE) %>% 
  summarise(n_female_legislators = sum(gender == "Female"), 
            n_male_legislators = sum(gender == "Male"),
            total_legislators = n(),
            prop_female_legislators = n_female_legislators/total_legislators,
            prop_male_legislators = n_male_legislators/total_legislators) %>% 
  ungroup() %>% 
  # Create variable to identify the year when each legislature started
  mutate(electoral_rule = "Proportional") %>% 
  mutate(start_yr = as.numeric(str_sub(period, start = 1, end = 4))) %>% 
  select(state, period, start_yr, everything())


## Remove objects from environment that wont be further used -----
remove(list = c("ags", "bc", "bcs", "camp", "chis", "chih",
                "cdmx", "coah", "col", "dgo", "mex", "gto",
                "gro", "hgo", "jal", "mich", "mor", "nay", 
                "nl", "oax", "pue", "qro", "qroo", "slp",
                "sin", "son", "tab", "tamps", "tlax", "ver",
                "yuc", "zac", "bd_leg_old"))


### Import data for the last two legislatures ----
bd_leg_new <- 
  read_excel("01_data/ch_3/electoral_results/Congreso estatales base de datos.xlsx",
           skip = 1) %>% 
  clean_names()

# Rename variables ----
bd_leg_new <- 
  bd_leg_new %>% 
  rename(state = x1,
         period = x2,
         party = x3,
         male_fptp = hombres_4,
         female_fptp = mujeres_5,
         male_pr = hombres_6,
         female_pr = mujeres_7,
         total = x8)

## Change data structure from wide to long ----
# bd_leg_new_l <- 
#   bd_leg_new %>% 
#   pivot_longer(-c(state:party, total),
#                names_to = "gender",
#                values_to = "n_seats") 
# 
# # Create variable electoral_rule and edit gender ----
# bd_leg_new_l <- 
#   bd_leg_new_l %>% 
#   mutate(electoral_rule = ifelse(str_detect(gender, "fptp"), "Mayority", "Proportional"),
#          gender = ifelse(str_detect(gender, "women"), "Female", "Male"))

## Calculate number and proportion of legislators by state, gender and legislature ----

# All ----
bd_leg_new_all <- 
  bd_leg_new %>% 
  group_by(state, period, .drop = FALSE) %>% 
  summarise(female_fptp = sum(female_fptp),
            female_pr = sum(female_pr),
            male_fptp = sum(male_fptp),
            male_pr = sum(male_pr)) %>% 
  mutate(n_female_legislators = female_fptp + female_pr, 
         n_male_legislators = male_fptp + male_pr) %>% 
  ungroup() %>% 
  select(-c(contains("fptp"), contains("_pr"))) %>% 
  mutate(total_legislators = n_female_legislators + n_male_legislators,
         prop_female_legislators = n_female_legislators/total_legislators,
         prop_male_legislators = n_male_legislators/total_legislators) %>% 
  ungroup() %>% 
  # Create variable to identify the year when each legislature started
  mutate(start_yr = as.numeric(str_sub(period, start = 1, end = 4))) %>% 
  select(state, period, start_yr, everything())

# Majority rule ----
bd_leg_new_mr <- 
  bd_leg_new %>% 
  group_by(state, period, .drop = FALSE) %>% 
  summarise(n_female_legislators = sum(female_fptp), 
            n_male_legislators = sum(male_fptp)) %>% 
  ungroup() %>% 
  mutate(total_legislators = n_female_legislators + n_male_legislators,
         prop_female_legislators = n_female_legislators/total_legislators,
         prop_male_legislators = n_male_legislators/total_legislators) %>% 
  ungroup() %>% 
  mutate(electoral_rule = "Majority") %>% 
  # Create variable to identify the year when each legislature started
  mutate(start_yr = as.numeric(str_sub(period, start = 1, end = 4))) %>% 
  select(state, period, start_yr, everything())


# Proportional representation ----
bd_leg_new_pr <- 
  bd_leg_new %>% 
  group_by(state, period, .drop = FALSE) %>% 
  summarise(n_female_legislators = sum(female_pr), 
            n_male_legislators = sum(male_pr)) %>% 
  ungroup() %>% 
  mutate(total_legislators = n_female_legislators + n_male_legislators,
         prop_female_legislators = n_female_legislators/total_legislators,
         prop_male_legislators = n_male_legislators/total_legislators) %>% 
  ungroup() %>% 
  mutate(electoral_rule = "Proportional") %>% 
  # Create variable to identify the year when each legislature started
  mutate(start_yr = as.numeric(str_sub(period, start = 1, end = 4))) %>% 
  select(state, period, start_yr, everything()) 


### Create final tibbles, binding data from old and new datasets ----

# All ----
bd_leg_all <- 
  bd_leg_old_propietary_all %>% 
  # Create n_legislature to later eliminate data for the last two legislatures
  group_by(state) %>% 
  mutate(n_legislature = rank(-start_yr)) %>% 
  ungroup() %>% 
  # Eliminate data for the last two legislatures
  filter(n_legislature > 2) %>% 
  select(-n_legislature) %>% 
  # Bind data from new dataset for the last two legislatures 
  bind_rows(bd_leg_new_all) %>% 
  # Arrange observations by state and start year
  arrange(state, start_yr) %>% 
  # Create lagged version of prop_female_legislators
  group_by(state) %>% 
  mutate(prop_female_legislators_lag = lag(prop_female_legislators)) %>% 
  ungroup() 


# Majority rule ----
bd_leg_mr <- 
  bd_leg_old_propietary_mr %>% 
  # Create n_legislature to later eliminate data for the last two legislatures
  group_by(state) %>% 
  mutate(n_legislature = rank(-start_yr)) %>% 
  ungroup() %>% 
  # Eliminate data for the last two legislatures
  filter(n_legislature > 2) %>% 
  select(-n_legislature) %>% 
  # Bind data from new dataset for the last two legislatures 
  bind_rows(bd_leg_new_mr) %>% 
  # Arrange observations by state and start year
  arrange(state, start_yr) %>% 
  # Create lagged version of prop_female_legislators
  group_by(state) %>% 
  mutate(prop_female_legislators_lag = lag(prop_female_legislators)) %>% 
  ungroup() 


# Proportional representation ----
bd_leg_pr <- 
  bd_leg_old_propietary_pr %>% 
  # Create n_legislature to later eliminate data for the last two legislatures
  group_by(state) %>% 
  mutate(n_legislature = rank(-start_yr)) %>% 
  ungroup() %>% 
  # Eliminate data for the last two legislatures
  filter(n_legislature > 2) %>% 
  select(-n_legislature) %>% 
  # Bind data from new dataset for the last two legislatures 
  bind_rows(bd_leg_new_pr) %>% 
  # Arrange observations by state and start year
  arrange(state, start_yr) %>% 
  # Create lagged version of prop_female_legislators
  group_by(state) %>% 
  mutate(prop_female_legislators_lag = lag(prop_female_legislators)) %>% 
  ungroup() 

