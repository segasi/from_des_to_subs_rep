### Load packages, define setup and vis theme ----
source("02_code/ch_3/00_pacakges_setup_theme.R")

### Import, process and join data bases ----
source("02_code/ch_3/06_join_data.R")


### Models 1 and 5 ----

# In these specifications:

# 1) Observations are nested by state
# 2) Fixed and random effects are estimated
# 3) In order to estimate the fixed effects, the following explanatory variables are included: i) ifreg, ii) prop_female_legislators_lag (a lagged version of the response variable, included to control for potential temporal correlation) and iii) party_alternation 
# 4) The following explanatory variable is included to estimate the random effects: i) ifreg

# Majority rule ----
m1_mr <- 
  glmer(cbind(n_female_legislators, n_male_legislators) ~ 1 + ifreg + prop_female_legislators_lag + party_alternation + (1 + ifreg | state), 
        data = bd_mr,
        family = "binomial")

summary(m1_mr)
fixef(m1_mr) # Fixed effects
ranef(m1_mr) # Random effects


# Proportional representation ----
m5_pr <- 
  glmer(cbind(n_female_legislators, n_male_legislators) ~ 1 + ifreg + prop_female_legislators_lag + party_alternation + (1 + ifreg | state), 
        data = bd_pr,
        family = "binomial")

summary(m5_pr)
fixef(m5_pr) # Fixed effects
ranef(m5_pr) # Random effects


### Models 2 and 6 ----

# In these specifications:

# 1) Observations are nested by state
# 2) Fixed and random effects are estimated
# 3) In order to estimate the fixed effects, the following explanatory variables are included: i) ifreg, ii) age_first_gender_law, iii) prop_female_legislators_lag and iv) party_alternation 
# 4) The following explanatory variable is included to estimate the random effects: i) ifreg

# Majority rule ----
m2_mr <- 
  glmer(cbind(n_female_legislators, n_male_legislators) ~ 1 + ifreg + age_first_gender_law + prop_female_legislators_lag + party_alternation + (1 + ifreg | state), 
        data = bd_mr,
        family = "binomial")

summary(m2_mr)
fixef(m2_mr) # Fixed effects
ranef(m2_mr) # Random effects


# Proportional representation ----
m6_pr <- 
  glmer(cbind(n_female_legislators, n_male_legislators) ~ 1 + ifreg + age_first_gender_law + prop_female_legislators_lag + party_alternation + (1 + ifreg | state),
        data = bd_pr,
        family = "binomial")

summary(m6_pr)
fixef(m6_pr) # Fixed effects
ranef(m6_pr) # Random effects



### Models 3 and 7 ----

# In these specifications:

# 1) Observations are nested by state
# 2) Fixed and random effects are estimated
# 3) In order to estimate the fixed effects, the following explanatory variables are included: i) ifreg, ii) age_first_gender_law, iii) state_gpd_per_capita_rescaled, iv) prop_female_legislators_lag and v) party_alternation 
# 4) The following explanatory variables are included to estimate the random effects: i) ifreg and ii) state_gpd_per_capita_rescaled

# Majority rule ----
m3_mr <- 
  glmer(cbind(n_female_legislators, n_male_legislators) ~ 1 + ifreg + age_first_gender_law + state_gpd_per_capita_rescaled + prop_female_legislators_lag + party_alternation + (1 + ifreg + state_gpd_per_capita_rescaled | state), 
        data = bd_mr,
        family = "binomial")

summary(m3_mr)
fixef(m3_mr) # Fixed effects
ranef(m3_mr) # Random effects


# Proportional representation ----
m7_pr <- 
  glmer(cbind(n_female_legislators, n_male_legislators) ~ 1 + ifreg + age_first_gender_law + state_gpd_per_capita_rescaled + prop_female_legislators_lag + party_alternation + (1 + ifreg + state_gpd_per_capita_rescaled | state), 
        data = bd_pr,
        family = "binomial")

summary(m7_pr)
fixef(m7_pr) # Fixed effects
ranef(m7_pr) # Random effects


### Models 4 and 8 ----

# In these specifications:

# 1) Observations are nested by state
# 2) Fixed and random effects are estimated
# 3) In order to estimate the fixed effects, the following explanatory variables are included: i) ifreg, ii) age_first_gender_law, iii) state_gpd_per_capita_rescaled, iv) percent_urban_pop, v) avg_yr_school, vi) prop_female_legislators_lag and vii) party_alternation 
# 4) The following explanatory variables are included to estimate the random effects: i) ifreg, ii) state_gpd_per_capita_rescaled, iii) percent_urban_pop and iv) avg_yr_school 



# Majority rule ----
m4_mr <- 
  glmer(cbind(n_female_legislators, n_male_legislators) ~ 1 + ifreg + age_first_gender_law + state_gpd_per_capita_rescaled + percent_urban_pop + avg_yr_school + prop_female_legislators_lag + party_alternation + (1 + ifreg + state_gpd_per_capita_rescaled + percent_urban_pop + avg_yr_school | state), 
        data = bd_mr,
        family = "binomial")

summary(m4_mr)
fixef(m4_mr) # Fixed effects
ranef(m4_mr) # Random effects


# Sin ifreg - modelo NO incluido en el artículo ----
m4_mr_a <- 
  glmer(cbind(n_female_legislators, n_male_legislators) ~ 1 +  age_first_gender_law + state_gpd_per_capita_rescaled + percent_urban_pop + avg_yr_school + prop_female_legislators_lag + party_alternation + (1 + state_gpd_per_capita_rescaled + percent_urban_pop + avg_yr_school | state), 
        data = bd_mr,
        family = "binomial")

summary(m4_mr_a)


# Proportional representation ----
m8_pr <- 
  glmer(cbind(n_female_legislators, n_male_legislators) ~ 1 + ifreg + age_first_gender_law + state_gpd_per_capita_rescaled + percent_urban_pop + avg_yr_school + prop_female_legislators_lag + party_alternation + (1 + ifreg + state_gpd_per_capita_rescaled + percent_urban_pop + avg_yr_school | state), 
        data = bd_pr,
        family = "binomial")

summary(m8_pr)
fixef(m8_pr) # Fixed effects
ranef(m8_pr) # Random effects


### Create results tables ----

# Majority rule models
stargazer(m1_mr, m2_mr, m3_mr, m4_mr, 
          title = "Results", align = TRUE, 
          type = "text", dep.var.caption = "Mayority rule", 
          out = "04_generated_data/ch_3/models/results_table_mr_models_20211109.txt")


# Proportional representation models
stargazer(m5_pr, m6_pr, m7_pr, m8_pr, 
          title = "Results", align = TRUE, 
          type = "text", dep.var.caption = "Proportional representation", 
          out = "04_generated_data/ch_3/models/results_table_pr_models_20211109.txt")


### Figure 3.5: predictions of the proportion of female legislators elected by majority rule and proportional representation using fixed effects of IFREG and age of the first gender law of models 4 and 8 ----

# IFREG 

pred_ef_m4_mr_ifreg <- 
  ggpredict(m4_mr, terms = c("ifreg"), type = "fixed") %>% 
  as_tibble() %>% 
  mutate(var_explicativa = "IFREG",
         principio_eleccion = "Mayority rule")

pred_ef_m4_mr_ifreg

pred_ef_m8_pr_ifreg <- 
  ggpredict(m8_pr, terms = c("ifreg"), type = "fixed") %>% 
  as_tibble() %>% 
  mutate(var_explicativa = "IFREG",
         principio_eleccion = "Proportional representation")

pred_ef_m8_pr_ifreg

# Age of the first gender law
pred_ef_m4_mr_antiguedad <- 
  ggpredict(m4_mr, terms = c("age_first_gender_law [all]")) %>% 
  as_tibble() %>% 
  mutate(var_explicativa = "Age of the first gender law",
         principio_eleccion = "Mayority rule")

pred_ef_m4_mr_antiguedad %>% print(n = Inf)

pred_ef_m8_pr_antiguedad <- 
  ggpredict(m8_pr, terms = c("age_first_gender_law [all]")) %>% 
  as_tibble() %>% 
  mutate(var_explicativa = "Age of the first gender law",
         principio_eleccion = "Proportional representation")

pred_ef_m8_pr_antiguedad %>% print(n = Inf)

# Join predictions
pred_ef <- 
  bind_rows(pred_ef_m4_mr_ifreg, 
            pred_ef_m8_pr_ifreg, 
            pred_ef_m4_mr_antiguedad, 
            pred_ef_m8_pr_antiguedad)

# Create graphs
g1 <- 
  pred_ef %>% 
  filter(var_explicativa == "IFREG") %>% 
  # For the Spanish version
  # mutate(principio_eleccion = ifelse(principio_eleccion == "Mayority rule", "Mayoría relativa", "Representación propocional")) %>% 
  ggplot(aes(x = x, 
             y =predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  facet_wrap(~ principio_eleccion) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-0.1, 5.1)) +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  # Spanish version
  # labs(title = "IFREG",
  #      x = "\nValores",
  #      y ="Proporciones predichas\n") +
  # English version
  labs(title = str_to_upper("IFREG"),
       x = "\nValues",
       y ="Predicted proportions\n") +
  theme_fdtsr + 
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0, size = 18, face = "bold.italic"),
        strip.text = element_text(size = 17),
        axis.title.x = element_text(size = 17),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing = unit(2, "lines"))


g2 <- 
  pred_ef %>% 
  filter(var_explicativa == "Age of the first gender law") %>%
  # For the Spanish version
  # mutate(principio_eleccion = ifelse(principio_eleccion == "Mayority rule", "Mayoría relativa", "Representación propocional")) %>% 
  ggplot(aes(x = x, 
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  facet_wrap(~ principio_eleccion) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-0.1, 22.2),
                     breaks = c(0, seq(5, 20, 5), 22)) +
  scale_y_continuous(breaks = seq(0, 0.5, 0.05)) +
  # Spanish version
  # labs(title = "Antigüedad de la primera norma de género",
  #      x = "\nAños",
  #      y = "Proporciones predichas\n") +
  # English version
  labs(title = "Age of the first gender law",
       x = "\nYears",
       y ="Predicted proportions\n") +
  theme_fdtsr + 
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0, size = 18, face = "bold.italic"),
        strip.text = element_text(size = 17),
        axis.title.x = element_text(size = 17),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing = unit(2, "lines"))


plot_grid(g1, g2, ncol = 1)

ggsave("03_vis/ch_3/figure_3.5_predictions_with_fixed_effects.png", width = 14, height = 9, dpi = 200)
ggsave("03_vis/ch_3/figure_3.5_predictions_with_fixed_effects.tiff", width = 14, height = 9, dpi = 200)

# Analysis of how the predicted proportion changes for different values of IFREG and majority rule
pred_ef %>% 
  filter(var_explicativa == "IFREG",
         principio_eleccion == "Mayority rule")

# Mean of avg_prop_female_legislators_lag anf 
bd_mr %>% 
  filter(!is.na(prop_female_legislators_lag)) %>% 
  summarise(avg_prop_female_legislators_lag = mean(prop_female_legislators_lag),
            avg_age_first_gender_law = mean(age_first_gender_law))

# Analysis of how the predicted proportion changes for different values of IFREG and proportional representation
pred_ef %>% 
  filter(var_explicativa == "IFREG",
         principio_eleccion == "Proportional representation")

# Mean of avg_prop_female_legislators_lag and age_first_gender_law
bd_pr %>% 
  filter(!is.na(prop_female_legislators_lag)) %>% 
  summarise(avg_prop_female_legislators_lag = mean(prop_female_legislators_lag),
            avg_age_first_gender_law = mean(age_first_gender_law))

# Analysis of how the predicted proportion changes for different values of Age of the first gender law and majority rule
pred_ef %>% 
  filter(var_explicativa == "Age of the first gender law",
         principio_eleccion == "Mayority rule") %>% 
  print(n = Inf)


bd_mr %>% 
  filter(!is.na(prop_female_legislators_lag)) %>% 
  summarise(avg_prop_female_legislators_lag = mean(prop_female_legislators_lag),
            avg_aifreg = mean(ifreg))


### Figure 3.6: predictions of the proportion of female legislators elected by majority rule considering fixed and random effects of IFREG in model 4 ----

# Predictions with fix effects
pred_ef_m4_mr_ifreg <- 
  ggpredict(m4_mr, terms = c("ifreg"), type = "fixed") %>% 
  as_tibble()

# Predictions with random effects
pred_ea_m4_mr_ifreg <- 
  ggpredict(m4_mr, 
            terms = c("ifreg", "state"), 
            type = "re") %>% 
  as_tibble()


pred_ea_m4_mr_ifreg %>%
  left_join(pred_ef_m4_mr_ifreg %>% 
              as_tibble() %>% 
              select(x, predicted_fijo = predicted), by = "x") %>%   
  ggplot() +
  geom_line(aes(x = x, y =predicted, group = group)) +
  geom_line(aes(x = x, y =predicted_fijo), color = "salmon") +
  scale_y_continuous(breaks = seq(0, .40, 0.05)) +
  facet_wrap(~ group, ncol = 8) +
  # Spanish version
  # labs(title = NULL,
  #      x = "\nValores",
  #      y ="Proporciones predichas\n") +
  # English version
  labs(title = NULL,
       x = "\nValues",
       y ="Predicted proportions\n") +
  theme_fdtsr + 
  theme(panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        panel.spacing = unit(0.5, "lines"),
        strip.text = element_text(size = 17))

ggsave("03_vis/ch_3/figure_3.6_predictions_fixed_and_random_effects_model_2_mr.png", width = 17, height = 11, dpi = 200)
ggsave("03_vis/ch_3/figure_3.6_predictions_fixed_and_random_effects_model_2_mr.tiff", width = 17, height = 11, dpi = 200)


pred_ea_m4_mr_ifreg %>%
  arrange(group, x) %>% 
  filter(x %in% c(0, 5)) %>% 
  group_by(group) %>% 
  mutate(cambio_0_a_5 = predicted - lag(predicted), 
         cambio_por_0_a_5 = (predicted - lag(predicted))/lag(predicted)*100) %>% 
  # filter(!is.na(cambio_0_a_5)) %>%
  # arrange(-cambio_por_0_a_5)  %>%
  print(n = Inf)



# Analysis of how the predicted proportion changes for different values of IFREG and majority rule
pred_ef %>% 
  filter(var_explicativa == "IFREG",
         principio_eleccion == "Mayority rule")

# Mean of avg_prop_female_legislators_lag anf 
bd_mr %>% 
  filter(!is.na(prop_female_legislators_lag)) %>% 
  summarise(avg_prop_female_legislators_lag = mean(prop_female_legislators_lag),
            avg_age_first_gender_law = mean(age_first_gender_law))
