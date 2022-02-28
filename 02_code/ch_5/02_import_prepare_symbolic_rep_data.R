### Load packages, define setup and vis theme ----
source("02_code/ch_3/00_pacakges_setup_theme.R")

### Import data ----

# This data base was created for chapter 4 and it already includes the variables create and used in chapter 3

bd_rep_sim <- 
  read_csv("04_generated_data/ch_4/bd_rep_simbólica_2.csv")

### Select relevant variables
bd_rep_sim <- 
  bd_rep_sim %>% 
  select(entidad_txt, legentidad, mujeres_tot, curules, age_first_gender_law, state_gpd_rescaled, party_alternation:avg_yr_school,
         starts_with("prop_"), 
         -c(age_first_gender_law, party_alternation, state_gpd_per_capita, year_info_inegi))
  

### Rename variables ----
bd_rep_sim <- 
  bd_rep_sim %>% 
  rename(state = entidad_txt,
         n_female_legislators = mujeres_tot,
         total_legislators = curules,
         # Share of occasions that the governing body was chaired by a female legislator in the corresponding legislative period
         shr_female_chair_gb = prop_od_presidentas,
         # Share of the governing body members that were female legislators in the corresponding legislative period
         shr_female_members_gb = prop_od_integrantes,
         # Share of committees that were chaired by a female legislator in the corresponding legislative period
         shr_female_chair_committees = prop_comisiones_presidentas,
         # Mean share of the committee members that were female legislators in the corresponding legislative period
         shr_female_members_committees = prop_comisiones_integrantes) 


### Generate and relocate new variables ----
bd_rep_sim <- 
  bd_rep_sim %>% 
  # Generate variable that records the legislative period
  mutate(leg_period = as.numeric(str_replace(legentidad, "Leg. ", "")),
         # Share of female legislators
         shr_female_legislators = n_female_legislators/total_legislators) %>% 
  ungroup() %>% 
  # Delete legentidad from tibble
  select(-legentidad) %>% 
  # Relocate variables
  relocate(leg_period, .after = state) %>% 
  relocate(shr_female_legislators, .after = total_legislators) 
