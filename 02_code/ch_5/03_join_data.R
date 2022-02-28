### Clean working environment ----
remove(list = ls())

### Load packages, define setup and vis theme ----
source("02_code/ch_5/00_pacakges_setup_theme.R")

### Import and prepare data ----
source("02_code/ch_5/01_import_prepare_sustantive_rep_data.R")
source("02_code/ch_5/02_import_prepare_symbolic_rep_data.R")

### Join bd_rep_sus_agg_fem and bd_rep_sim ----
bd_rep_sus_agg_fem <- 
  bd_rep_sus_agg_fem %>% 
  left_join(bd_rep_sim, by = c("state", "leg_period"))

### Save tibble in files with .csv format ----
bd_rep_sus_agg_fem %>% 
  write_csv("04_generated_data/ch_5/bd_rep_sus_agg_fem.csv")
