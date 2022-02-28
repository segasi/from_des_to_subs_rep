### Load packages, define setup and vis theme ----
source("02_code/ch_3/00_pacakges_setup_theme.R")

### Import state data ----

ags_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Aguascalientes_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Aguascalientes", 
         leg_period = 2)  
 

bc_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Baja California_Representación sustantiva.xlsx",
             sheet = "2016-2019") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Baja California", 
         leg_period = 1) 


bc_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Baja California_Representación sustantiva.xlsx",
             sheet = "2019-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Baja California", 
         leg_period = 2)

camp_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Campeche_Representación sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Campeche", 
         leg_period = 1) 

camp_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Campeche_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Campeche", 
         leg_period = 2) 

chis_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Chiapas_Representación sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Chiapas", 
         leg_period = 1) 

chis_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Chiapas_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Chiapas", 
         leg_period = 2)

chih_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Chihuahua_Representación sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Chihuahua", 
         leg_period = 1)

chih_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Chihuahua_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
    mutate(state = "Chihuahua", 
         leg_period = 2)

cdmx_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/CDMX_Representación sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Ciudad de México", 
         leg_period = 1) 

cdmx_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/CDMX_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Ciudad de México", 
         leg_period = 2)

coah_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Coahuila_Representación sustantiva.xlsx",
             sheet = "2015-2017") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Coahuila", 
         leg_period = 1,
         fecha_p = as.character(fecha_p),
         archivo = as.character(archivo))

coah_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Coahuila_Representación sustantiva.xlsx",
             sheet = "2018-2020") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Coahuila", 
         leg_period = 2)

col_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Colima_Representación sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Colima", 
         leg_period = 1)

col_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Colima_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Colima", 
         leg_period = 2)

mex_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/México_Representación sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Estado de México", 
         leg_period = 1)

mex_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/México_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Estado de México", 
         leg_period = 2)

gto_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Guanajuato_Representación sustantiva.xlsx",
             sheet = "2015-2018. ") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Guanajuato", 
         leg_period = 1)

gto_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Guanajuato_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  select(-c("nombre_de_la_norma", "tipo_norma")) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Guanajuato", 
         leg_period = 2)

hgo_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Hidalgo_Representación sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Hidalgo", 
         leg_period = 1, 
         fecha_p = as.character(fecha_p))

hgo_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Hidalgo_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Hidalgo", 
         leg_period = 2)

jal_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Jalisco_Representación sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Jalisco", 
         leg_period = 1) 

jal_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Jalisco_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Jalisco", 
         leg_period = 2)

mich_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Michoacán .xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Michoacán", 
         leg_period = 1)

mich_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Michoacán .xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Michoacán", 
         leg_period = 2)

mor_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Morelos_Representación sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Morelos", 
         leg_period = 1) 

mor_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Morelos_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Morelos", 
         leg_period = 2)

nay_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Nayarit_ Representación Sustantiva.xlsx",
             sheet = "2014-2017") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Nayarit", 
         leg_period = 1)

nay_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Nayarit_ Representación Sustantiva.xlsx",
             sheet = "2017-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Nayarit", 
         leg_period = 2)


nl_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Nuevo León_Representación Sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Nuevo León", 
         leg_period = 1)

nl_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Nuevo León_Representación Sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Nuevo León", 
         leg_period = 2)

oax_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Oaxaca_Representación sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>%
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Oaxaca", 
         leg_period = 1)

oax_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Oaxaca_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Oaxaca", 
         leg_period = 2)

pue_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Puebla_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Puebla", 
         leg_period = 2,
         fecha_p = as.character(fecha_p),
         archivo = as.character(archivo))
  

qro_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Querétaro_Representación sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Querétaro", 
         leg_period = 1)

qro_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Querétaro_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Querétaro", 
         leg_period = 2)

qroo_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Quintana roo_Representación sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Quintana Roo", 
         leg_period = 1)

qroo_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Quintana roo_Representación sustantiva.xlsx",
             sheet = "2019-2022") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Quintana Roo", 
         leg_period = 2)

slp_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/San Luis Potosí_ Representación sustantiva.xlsx",
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "San Luis Potosí", 
         leg_period = 1)

slp_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/San Luis Potosí_ Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "San Luis Potosí", 
         leg_period = 2,
         archivo = as.character(archivo))

sin_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Sinaloa_Representación sustantiva.xlsx",
             sheet = "2016-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Sinaloa", 
         leg_period = 1)

sin_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Sinaloa_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Sinaloa", 
         leg_period = 2)

son_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Sonora_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Sonora", 
         leg_period = 2)

tab_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Tabasco_Representación sustantiva.xlsx",
             sheet = "2016-2018") %>% 
  clean_names() %>% # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Tabasco", 
         leg_period = 1,
         archivo = as.character(archivo))

tab_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Tabasco_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Tabasco", 
         leg_period = 2,
         archivo = as.character(archivo)) 

tamps_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Tamaulipas_Representación sustantiva.xlsx",
             sheet = "2016-2019") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Tamaulipas", 
         leg_period = 1)

tamps_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Tamaulipas_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Tamaulipas", 
         leg_period = 2)

tlax_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Tlaxcala_Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Tlaxcala", 
         leg_period = 2)

ver_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Veracruz_ Representación sustantiva.xlsx",
             sheet = "2016-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Veracruz", 
         leg_period = 1)

ver_2 <-
  read_excel("01_data/ch_5/sustantive_representation/Veracruz_ Representación sustantiva.xlsx",
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Veracruz", 
         leg_period = 2) 
  
yuc_1 <- 
  read_excel("01_data/ch_5/sustantive_representation/Yucatán_Representación sustantiva.xlsx", 
             sheet = "2015-2018") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Yucatán", 
         leg_period = 1) 
  

yuc_2 <- 
  read_excel("01_data/ch_5/sustantive_representation/Yucatán_Representación sustantiva.xlsx", 
             sheet = "2018-2021") %>% 
  clean_names() %>% 
  # Select variables of interest
  select(iniciativa:feminista) %>% 
  # Create variables to later identify the state and legislative period
  mutate(state = "Yucatán", 
         leg_period = 2)
  

## Join all tibbles loaded so far and create a new one called bd_rep_sus ----
bd_rep_sus <- 
  bind_rows(ags_2, bc_1, bc_2, camp_1, camp_2, 
            chis_1, chis_2, chih_1, chih_2, cdmx_1, 
            cdmx_2, coah_1, coah_2, col_1, col_2, 
            mex_1, mex_2, gto_1, gto_2, hgo_1, 
            hgo_2, jal_1, jal_2, mich_1, mich_2, 
            mor_1, mor_2, nay_1, nay_2, nl_1, 
            nl_2, oax_1, oax_2, pue_2, qro_1, 
            qro_2, qroo_1, qroo_2, slp_1, slp_2, 
            sin_1, sin_2, son_2, tab_1, tab_2, 
            tamps_1, tamps_2, tlax_2, ver_1, ver_2, 
            yuc_1, yuc_2)

## Rename and reorder variables ----
bd_rep_sus <- 
  bd_rep_sus %>%
  rename(bill_short_name = iniciativa,
         bill_name = titulo,
         gender = genero,
         name = nombre,
         party = partido,
         presentation_date = fecha_p,
         voted_date = fecha_v,
         status = estatus,
         norm_type = tipo_de_norma,
         norm = norma,
         topic = tema,
         file = archivo,
         constituency = poblacion,
         impact = impacto,
         expansion = ampliacion,
         equality = igualdad,
         feminist = feminista) %>% 
  select(state, leg_period, everything())


## Homogenize values for variables gender, party, status norm_type, topic, constituency, impact, agenda, expansion, equality and feminist ----
bd_rep_sus <- 
  bd_rep_sus %>% 
  mutate(gender = case_when(gender == "Mujer" ~ "Female",
                            gender == "Grupo de Mujeres" ~ "Female group",
                            gender == "Grupo mujeres" ~ "Female group",
                            gender == "Grupo Mixto" ~ "Mixed group",
                            gender == "Grupo mixto" ~ "Mixed group",
                            gender == "Mixto" ~ "Mixed group",
                            gender == "Grupos hombres" ~ "Male group",
                            gender == "Hombre" ~ "Male",
                            T ~ gender),
         party = case_when(party == "INDEOENDIENTE" ~ "Independent",
                           party == "INDEPENDIENTE" ~ "Independent",
                           party == "LOCAL" ~ "State party",
                           party == "MORENA" ~ "Morena",
                           party == "mo" ~ "Morena",
                           party == "COALICIÓN" ~ "Coalition",
                           str_detect(party, "CIUDADANA")  ~ "Citizen initiative",
                           T ~ party),
         status = case_when(status == "Aprobada" ~ "Approved",
                            status == "Rechazada" ~ "Not approved",
                            status == "Pendiente" ~ "Pending"),
         norm_type = case_when(norm_type == "Ambos" ~ "Constitution and ordinary law",
                               norm_type ==  "Constitucion" ~ "Constitution",
                               norm_type ==  "Constitución" ~ "Constitution",
                               norm_type ==  "Ley ordinaria" ~ "Ordinary law",
                               norm_type ==  "Ley Ordinaria" ~ "Ordinary law"),
         topic = case_when(str_detect(topic, "Bienestar") ~ "Social welfare",
                           str_detect(topic, "gualdad de ") ~ "Gender equality",
                           str_detect(topic, "Educac") ~ "Education",
                           str_detect(topic, "Econom") ~ "Economy",
                           str_detect(topic, "Famili") ~ "Family",
                           str_detect(topic, "Ley de |Otro") ~ "Other",
                           str_detect(topic, "electoral de ") ~ "Electoral gender regime",
                           str_detect(topic, "mbiente") ~ "Environment",
                           str_detect(topic, "Salud") ~ "Health",
                           str_detect(topic, "Seguridad") ~ "Security",
                           str_detect(topic, "Sistema ") ~ "Political system",
                           T ~ topic),
         constituency = case_when(str_detect(constituency, "os mayo") ~ "Elderly people",
                                  str_detect(constituency, "sidad sexoge|sidad Sexoge") ~ "Sex-Generic Diversity",
                                  str_detect(constituency, "eneral") ~ "General",
                                  str_detect(constituency, "ndígenas") ~ "Indigenous people",
                                  str_detect(constituency, "ujeres") ~ "Women",
                                  str_detect(constituency, "ñez y juventud|ñez y Juventud|ños y juventudes") ~ "Childhood and youth",
                                  str_detect(constituency, "discapacidad") ~ "People with disabilities",
                                  T ~ constituency),
         impact = case_when(str_detect(impact, "S") ~ "Yes",
                            T ~ impact),
         agenda = case_when(str_detect(agenda, "Civiles") ~ "Civil rights",
                            str_detect(agenda, "Educativos") ~ "Education rights",
                            str_detect(agenda, "Laborales") ~ "Labor rights",
                            str_detect(agenda, "Politicos|Políticos") ~ "Political rights",
                            str_detect(agenda, "Libertad de") ~ "Freedom from violence",
                            str_detect(agenda, "No c") ~ "Not classifiable",
                            str_detect(agenda, "o re") ~ "Does not reflect",
                            str_detect(agenda, "Reproductivos") ~ "Reproductive rights",
                            str_detect(agenda, "Salud") ~ "Health rights",
                            str_detect(agenda, "Social") ~ "Social rights",
                            T ~ agenda),
         expansion = case_when(str_detect(expansion, "Amplia") ~ "Expands",
                               str_detect(expansion, "No afecta") ~ "No change",
                               str_detect(expansion, "Restringe") ~ "Restricts",
                               T ~ expansion),
         equality = case_when(str_detect(equality, "S") ~ "Yes",
                              T ~ equality),
         feminist = case_when(str_detect(feminist, "S") ~ "Yes",
                              T ~ feminist))


## Change variable type of presentation_date ----
bd_rep_sus <- 
  bd_rep_sus %>% 
  mutate(presentation_date = ifelse(presentation_date == "n/a", NA, presentation_date),
         presentation_date = ifelse(presentation_date == "N/A", NA, presentation_date),
         presentation_date = ifelse(presentation_date == "N/a", NA, presentation_date),
         presentation_date = ifelse(presentation_date == "s/f", NA, presentation_date),
         presentation_date = ifelse(presentation_date == "nd", NA, presentation_date),
         presentation_date = ifelse(presentation_date == "No hay dato", NA, presentation_date),
         presentation_date = str_replace(presentation_date, "\\.0", ""),
         presentation_date = str_pad(presentation_date, width = 6, pad = "0"),
         # The following changes where made after manually checking the corresponding file to determine what was the previos and/or posterior date
         presentation_date = case_when(presentation_date == "1103200" ~ "110320", # Puebla
                                       presentation_date == "007118" ~ "071118", # Puebla
                                       presentation_date == "009719" ~ "090719", # Oaxaca
                                       presentation_date == "022021" ~ "020221", # Oaxaca
                                       presentation_date == "151920" ~ "150920", # Coahuila
                                       presentation_date == "1111218" ~ "111218", # Coahuila
                                       presentation_date == "231616" ~ "231116", # Colima
                                       presentation_date == "3100816" ~ "310816", # Jalisco
                                       presentation_date == "3010816" ~ "310816", # Jalisco
                                       presentation_date == "0610116" ~ "061016", # Jalisco
                                       presentation_date == "153018" ~ "150318", # Jalisco
                                       presentation_date == "012019" ~ "120619", # Jalisco
                                       presentation_date == "151917" ~ "150917", # Nayarit
                                       presentation_date == "290217" ~ "280217", # Nuevo León
                                       presentation_date == "290219" ~ "280219", # Nuevo León
                                       presentation_date == "102021" ~ "100221", # Querétaro
                                       presentation_date == "201617" ~ "200617", # Sinaloa
                                       presentation_date == "024092" ~ "240920", # Sonora
                                       presentation_date == "022119" ~ "221119", # Sonora
                                       presentation_date == "2100217" ~ "210217", # Yucatán
                                       presentation_date == "013119" ~ "131119", # Yucatán
                                       T ~ presentation_date),
         presentation_date = dmy(presentation_date))


### Remove bills with NAs in gender (3 obs), feminist (2 obs), status (11 obs), impact (1 obs), agenda (2 obs) ----
bd_rep_sus <- 
  bd_rep_sus %>% 
  filter(!is.na(gender),
         !is.na(feminist),
         !is.na(status),
         !is.na(impact),
         !is.na(agenda))


### Remove bills from Aguascalientes, Morelos, Puebla, Sonora and Tlaxcala ----

# In the case of Aguascalientes, Morelos, Puebla, Sonora and Tlaxcala the bills are removed because we only have data for one legislative period. In the case of Morelos the bills are removed due to lack of information for the second legislative period

# This steps removes 2,406 bills
bd_rep_sus <- 
  bd_rep_sus %>% 
  filter(!state %in% c("Aguascalientes", "Morelos", "Puebla", "Sonora", "Tlaxcala"))


### Create variable gender_gral ----

# This variable is needed in order to group the bills presented by individual female legislators and groups of female legislators, as well as those presented by individual male legislators and groups of male legislators
bd_rep_sus <- 
  bd_rep_sus %>% 
  mutate(gender_gral = case_when(str_detect(gender, "Female") ~ "Female",
                               str_detect(gender, "Male") ~ "Male",
                               T ~ gender)) 




### Generate bd_rep_sus_agg ----

# This tibble contains variables aggregated by state, legislative period and gender_gral

bd_rep_sus_agg <- 
  bd_rep_sus %>% 
  # Group by state, legislative period and gender_gral
  group_by(state, leg_period, gender_gral) %>% 
  summarise(bills = n(), # No. of bills presented
            # No. of bills approved
            bills_pass = sum(status == "Approved", na.rm = T), 
            # No. of feminist bills presented 
            bills_fem = sum(feminist == "Yes", na.rm = T), 
            # No. of feminist bills approved 
            bills_fem_pass = sum(feminist == "Yes" & status == "Approved", na.rm = T)) %>%
  ungroup() %>%  
  # Group by state and legislative period
  group_by(state, leg_period) %>% 
  mutate(bills_tot = sum(bills), # Total number of bills presented
         # Total number of bills that were approved
         bills_pass_tot = sum(bills_pass),
         # Total number of feminist bills
         bills_fem_tot = sum(bills_fem),
         # Total number of feminist bills that were approved
         bills_fem_pass_tot = sum(bills_fem_pass)) %>% 
  ungroup()


### Generate tibble only with aggregated data for female legislators ----
bd_rep_sus_agg_fem <- 
  bd_rep_sus_agg %>% 
  filter(gender_gral == "Female") %>% 
  select(-gender_gral)

### Generate variables that register the number of bills that complement what the females legislators did ----

# This variables will be used in the models to specify the number of "failures" within cbind() + glm(..., family = binomial(link = "logit"))

bd_rep_sus_agg_fem <- 
  bd_rep_sus_agg_fem %>% 
  # Number of bills presented by non female legislators (i.e., the number of failures).
  mutate(bills_others = bills_tot - bills, 
         # Number of feminist bills presented by non-female legislators
         bills_fem_others = bills_fem_tot - bills_fem,
         # Number of bills presented by female legislators that were not approved
         bills_not_pass = bills - bills_pass,
         # Number of feminist bills presented by female legislators that were not approved
         bills_fem_not_pass = bills_fem - bills_fem_pass,
         # Number of bills presented by non-female legislators that were approved
         bills_pass_others = bills_pass_tot - bills_pass,
         # Number of feminist bills presented by non-female legislators that were approved
         bills_fem_others_pass = bills_fem_pass_tot - bills_fem_pass
         ) 
