### Clean working environment ----
remove(list = ls())

### Run 03_join_data script.R ----

# This will load packages, define setup and vis themes, import and prepare data

source("02_code/ch_5/03_join_data.R")


### Models 1 to 5 ----


# In these specifications:

# 1) The response variable is the share of bills presented by female legislators with respect to the total number of bills presented, defined in every case as cbind(bills, bills_others). This is so because when using cbind() inside glm() with family = binomial(link = "logit"), R expects the first figure to be the number of successes and the second the number of failures. Here bills_others is the number of bills presented by non female legislators (i.e., the number of failures).
# 2) Observations are nested by state and legislative period, specified in every case as (1 | state/leg_period)
# 3) Fixed and random effects are estimated
# 4) In order to estimate the fixed effects, the following explanatory variables are included: i) shr_female_legislators, ii) state_gpd_rescaled, iii) percent_urban_pop, iv) avg_yr_school 
# 5) In models 2 to 5 we also include one of the following explanatory variables that measure symbolic representation


## Model 1 ----
m1 <- 
  glmer(cbind(bills, bills_others) ~ 1 + shr_female_legislators + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m1)
fixef(m1) # Fixed effects
ranef(m1) # Random effects

# ggpredict(m1, 
#           terms = c("shr_female_legislators [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>%  
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(0.31, 0.66),
#                      breaks = c(0.32, seq(0.35, 0.6, 0.05),0.65)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(title = "IV: Share of female representatives",
#        x = "\nShare",
#        y = "Predicted proportions\n") +
#   theme_fdtsr

## Model 2 ----

# Adding the share of occasions that the governing body was chaired by a female legislator in the corresponding legislative period (shr_female_chair_gb) as an explanatory variable 

m2 <- 
  glmer(cbind(bills, bills_others) ~ 1 + shr_female_legislators + shr_female_chair_gb + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m2)
fixef(m2) # Fixed effects
ranef(m2) # Random effects


# ggpredict(m2, 
#           terms = c("shr_female_chair_gb [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-0.01, 0.76),
#                      breaks = seq(0, 0.9, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.025)) +
#   labs(title = "IV: Share of occasions that the governing body was chaired by a female legislator in the\ncorresponding legislative period",
#        
#        x = "\nShare",
#        y = "Predicted proportions\n") +
#   theme_fdtsr


## Model 3 ----

# Adding the share of the governing body members that were female legislators (shr_female_members_gb) as an explanatory variable 

m3 <- 
  glmer(cbind(bills, bills_others) ~ 1 + shr_female_legislators + shr_female_members_gb + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m3)
fixef(m3) # Fixed effects
ranef(m3) ## Random effects


# ggpredict(m3, 
#           terms = c("shr_female_members_gb [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0.15, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Share of the governing body members that were female legislators",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr



## Model 4 ----

# Adding the share of committees that were chaired by a female legislator in the corresponding legislative period (shr_female_chair_committees) as an explanatory variable 

m4 <- 
  glmer(cbind(bills, bills_others) ~ 1 + shr_female_legislators + shr_female_chair_committees + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m4)
fixef(m4) # Fixed effects
ranef(m4) ## Random effects


# ggpredict(m4, 
#           terms = c("shr_female_chair_committees [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0.15, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Share of committees that were chaired by a female legislator in the corresponding legislative period",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr


## Model 5 ----

# Adding the mean share of the committee members that were female legislators (shr_female_members_committees) as an explanatory variable 

m5 <- 
  glmer(cbind(bills, bills_others) ~ 1 + shr_female_legislators + shr_female_members_committees + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m5)
fixef(m5) # Efectos fijos
ranef(m5) # Efectos aleatorios


# ggpredict(m5, 
#           terms = c("shr_female_members_committees [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Mean share of the committee members that were female legislators",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr



### Models 6 to 10 ----

# In these specifications:

# 1) The response variable is the share of feminist bills presented by female legislators with respect to the total number of feminist bills presented, defined in every case as cbind(bills_fem, bills_fem_others). This is so because when using cbind() inside glm() with family = binomial(link = "logit"), R expects the first figure to be the number of successes and the second the number of failures. Here bills_fem_others is the number of feminist bills presented by non-female legislators (i.e., the number of failures).
# 2) Observations are nested by state and legislative period, specified in every case as (1 | state/leg_period)
# 3) Fixed and random effects are estimated
# 4) In order to estimate the fixed effects, the following explanatory variables are included: i) shr_female_legislators, ii) state_gpd_rescaled, iii) percent_urban_pop, iv) avg_yr_school 
# 5) In models 2 to 5 we also include one of the following explanatory variables that measure symbolic representation


## Model 6 ----
m6 <- 
  glmer(cbind(bills_fem, bills_fem_others) ~ 1 + shr_female_legislators + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m6)
fixef(m6) # Fixed effects
ranef(m6) # Random effects

# ggpredict(m6, 
#           terms = c("shr_female_legislators [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>%  
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(0.31, 0.66),
#                      breaks = c(0.32, seq(0.35, 0.6, 0.05),0.65)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(title = "IV: Share of female representatives",
#        x = "\nShare",
#        y = "Predicted proportions\n") +
#   theme_fdtsr

## Model 7 ----

# Adding the share of occasions that the governing body was chaired by a female legislator in the corresponding legislative period (shr_female_chair_gb) as an explanatory variable 

m7 <- 
  glmer(cbind(bills_fem, bills_fem_others) ~ 1 + shr_female_legislators + shr_female_chair_gb + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m7)
fixef(m7) # Fixed effects
ranef(m7) # Random effects


# ggpredict(m7, 
#           terms = c("shr_female_chair_gb [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-0.01, 0.76),
#                      breaks = seq(0, 0.9, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.025)) +
#   labs(title = "IV: Share of occasions that the governing body was chaired by a female legislator in the\ncorresponding legislative period",
#        
#        x = "\nShare",
#        y = "Predicted proportions\n") +
#   theme_fdtsr


## Model 8 ----

# Adding the share of the governing body members that were female legislators (shr_female_members_gb) as an explanatory variable 

m8 <- 
  glmer(cbind(bills_fem, bills_fem_others) ~ 1 + shr_female_legislators + shr_female_members_gb + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m8)
fixef(m8) # Fixed effects
ranef(m8) ## Random effects


# ggpredict(m8, 
#           terms = c("shr_female_members_gb [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0.15, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Share of the governing body members that were female legislators",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr



## Model 9 ----

# Adding the share of committees that were chaired by a female legislator in the corresponding legislative period (shr_female_chair_committees) as an explanatory variable 

m9 <- 
  glmer(cbind(bills_fem, bills_fem_others) ~ 1 + shr_female_legislators + shr_female_chair_committees + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m9)
fixef(m9) # Fixed effects
ranef(m9) ## Random effects


# ggpredict(m9, 
#           terms = c("shr_female_chair_committees [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0.15, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Share of committees that were chaired by a female legislator in the corresponding legislative period",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr


## Model 10 ----

# Adding the mean share of the committee members that were female legislators (shr_female_members_committees) as an explanatory variable 

m10 <- 
  glmer(cbind(bills_fem, bills_fem_others) ~ 1 + shr_female_legislators + shr_female_members_committees + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m10)
fixef(m10) # Efectos fijos
ranef(m10) # Efectos aleatorios


# ggpredict(m10, 
#           terms = c("shr_female_members_committees [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Mean share of the committee members that were female legislators",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr



### Models 11 to 15 ----

# In these specifications:

# 1) The response variable is the share of bills presented by female legislators that were approved with respect to the total number of bills presented by female legislators, defined in every case as cbind(bills_pass, bills_not_pass). This is so because when using cbind() inside glm() with family = binomial(link = "logit"), R expects the first figure to be the number of successes and the second the number of failures. Here bills_not_pass is the number of bills presented by female legislators that were not approved (i.e., the number of failures).

# 2) Observations are nested by state and legislative period, specified in every case as (1 | state/leg_period)
# 3) Fixed and random effects are estimated
# 4) In order to estimate the fixed effects, the following explanatory variables are included: i) shr_female_legislators, ii) state_gpd_rescaled, iii) percent_urban_pop, iv) avg_yr_school 
# 5) In models 2 to 5 we also include one of the following explanatory variables that measure symbolic representation


## Model 11 ----
m11 <- 
  glmer(cbind(bills_pass, bills_not_pass) ~ 1 + shr_female_legislators + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m11)
fixef(m11) # Fixed effects
ranef(m11) # Random effects

# ggpredict(m11,
#           terms = c("shr_female_legislators [all]"),
#           type = "fixed", nsim = 5000) %>%
#   as_tibble() %>%  
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(0.31, 0.66),
#                      breaks = c(0.32, seq(0.35, 0.6, 0.05),0.65)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(title = "IV: Share of female representatives",
#        x = "\nShare",
#        y = "Predicted proportions\n") +
#   theme_fdtsr

## Model 12 ----

# Adding the share of occasions that the governing body was chaired by a female legislator in the corresponding legislative period (shr_female_chair_gb) as an explanatory variable 

m12 <- 
  glmer(cbind(bills_pass, bills_not_pass) ~ 1 + shr_female_legislators + shr_female_chair_gb + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m12)
fixef(m12) # Fixed effects
ranef(m12) # Random effects


# ggpredict(m12, 
#           terms = c("shr_female_chair_gb [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-0.01, 0.76),
#                      breaks = seq(0, 0.9, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.025)) +
#   labs(title = "IV: Share of occasions that the governing body was chaired by a female legislator in the\ncorresponding legislative period",
#        
#        x = "\nShare",
#        y = "Predicted proportions\n") +
#   theme_fdtsr


## Model 13 ----

# Adding the share of the governing body members that were female legislators (shr_female_members_gb) as an explanatory variable 

m13 <- 
  glmer(cbind(bills_pass, bills_not_pass) ~ 1 + shr_female_legislators + shr_female_members_gb + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m13)
fixef(m13) # Fixed effects
ranef(m13) ## Random effects


# ggpredict(m13, 
#           terms = c("shr_female_members_gb [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0.15, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Share of the governing body members that were female legislators",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr



## Model 14 ----

# Adding the share of committees that were chaired by a female legislator in the corresponding legislative period (shr_female_chair_committees) as an explanatory variable 

m14 <- 
  glmer(cbind(bills_pass, bills_not_pass) ~ 1 + shr_female_legislators + shr_female_chair_committees + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m14)
fixef(m14) # Fixed effects
ranef(m14) ## Random effects

# Predicciones para shr_female_chair_committees
# ggpredict(m14,
#           terms = c("shr_female_chair_committees [all]"),
#           type = "fixed", nsim = 5000) %>%
#   as_tibble() %>%
#   print(n = Inf)

# ggpredict(m14, 
#           terms = c("shr_female_chair_committees [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0.15, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Share of committees that were chaired by a female legislator in the corresponding legislative period",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr


## Model 15 ----

# Adding the mean share of the committee members that were female legislators (shr_female_members_committees) as an explanatory variable 

m15 <- 
  glmer(cbind(bills_pass, bills_not_pass) ~ 1 + shr_female_legislators + shr_female_members_committees + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m15)
fixef(m15) # Efectos fijos
ranef(m15) # Efectos aleatorios


# ggpredict(m15, 
#           terms = c("shr_female_members_committees [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Mean share of the committee members that were female legislators",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr



### Models 16 to 20 ----

# In these specifications:

# 1) The response variable is the share of feminist bills presented by female legislators that were approved with respect to the total number of feminist bills presented by female legislators, defined in every case as cbind(bills_fem_pass, bills_fem_not_pass). This is so because when using cbind() inside glm() with family = binomial(link = "logit"), R expects the first figure to be the number of successes and the second the number of failures. Here bills_fem_not_pass is the number of feminist bills presented by female legislators that were not approved (i.e., the number of failures).

# 2) Observations are nested by state and legislative period, specified in every case as (1 | state/leg_period)
# 3) Fixed and random effects are estimated
# 4) In order to estimate the fixed effects, the following explanatory variables are included: i) shr_female_legislators, ii) state_gpd_rescaled, iii) percent_urban_pop, iv) avg_yr_school 
# 5) In models 2 to 5 we also include one of the following explanatory variables that measure symbolic representation


## Model 16 ----
m16 <- 
  glmer(cbind(bills_fem_pass, bills_fem_not_pass) ~ 1 + shr_female_legislators + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m16)
fixef(m16) # Fixed effects
ranef(m16) # Random effects

# ggpredict(m16, 
#           terms = c("shr_female_legislators [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>%  
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(0.31, 0.66),
#                      breaks = c(0.32, seq(0.35, 0.6, 0.05),0.65)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(title = "IV: Share of female representatives",
#        x = "\nShare",
#        y = "Predicted proportions\n") +
#   theme_fdtsr

## Model 17 ----

# Adding the share of occasions that the governing body was chaired by a female legislator in the corresponding legislative period (shr_female_chair_gb) as an explanatory variable 

m17 <- 
  glmer(cbind(bills_fem_pass, bills_fem_not_pass) ~ 1 + shr_female_legislators + shr_female_chair_gb + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m17)
fixef(m17) # Fixed effects
ranef(m17) # Random effects


# ggpredict(m17, 
#           terms = c("shr_female_dhair_gb [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-0.01, 0.76),
#                      breaks = seq(0, 0.9, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.025)) +
#   labs(title = "IV: Share of occasions that the governing body was chaired by a female legislator in the\ncorresponding legislative period",
#        
#        x = "\nShare",
#        y = "Predicted proportions\n") +
#   theme_fdtsr


## Model 18 ----

# Adding the share of the governing body members that were female legislators (shr_female_members_gb) as an explanatory variable 

m18 <- 
  glmer(cbind(bills_fem_pass, bills_fem_not_pass) ~ 1 + shr_female_legislators + shr_female_members_gb + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m18)
fixef(m18) # Fixed effects
ranef(m18) ## Random effects


# ggpredict(m18, 
#           terms = c("shr_female_members_gb [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0.15, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Share of the governing body members that were female legislators",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr



## Model 19 ----

# Adding the share of committees that were chaired by a female legislator in the corresponding legislative period (shr_female_chair_committees) as an explanatory variable 

m19 <- 
  glmer(cbind(bills_fem_pass, bills_fem_not_pass) ~ 1 + shr_female_legislators + shr_female_chair_committees + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m19)
fixef(m19) # Fixed effects
ranef(m19) ## Random effects


# ggpredict(m19, 
#           terms = c("shr_female_chair_committees [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0.15, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Share of committees that were chaired by a female legislator in the corresponding legislative period",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr


## Model 20 ----

# Adding the mean share of the committee members that were female legislators (shr_female_members_committees) as an explanatory variable 

m20 <- 
  glmer(cbind(bills_fem_pass, bills_fem_not_pass) ~ 1 + shr_female_legislators + shr_female_members_committees + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m20)
fixef(m20) # Efectos fijos
ranef(m20) # Efectos aleatorios


# ggpredict(m20, 
#           terms = c("shr_female_members_committees [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Mean share of the committee members that were female legislators",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr



### Models 21 to 25 ----

# In these specifications:

# 1) The response variable is the share of bills presented by female legislators that were approved with respect to the total number of bills approved, defined in every case as cbind(bills_pass, bills_pass_others). This is so because when using cbind() inside glm() with family = binomial(link = "logit"), R expects the first figure to be the number of successes and the second the number of failures. Here bills_pass_others is the number of bills presented by non-female legislators that were approved (i.e., the number of failures).
# 2) Observations are nested by state and legislative period, specified in every case as (1 | state/leg_period)
# 3) Fixed and random effects are estimated
# 4) In order to estimate the fixed effects, the following explanatory variables are included: i) shr_female_legislators, ii) state_gpd_rescaled, iii) percent_urban_pop, iv) avg_yr_school 
# 5) In models 2 to 5 we also include one of the following explanatory variables that measure symbolic representation


## Model 21 ----
m21 <- 
  glmer(cbind(bills_pass, bills_pass_others) ~ 1 + shr_female_legislators + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m21)
fixef(m21) # Fixed effects
ranef(m21) # Random effects

# ggpredict(m21, 
#           terms = c("shr_female_legislators [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>%  
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(0.31, 0.66),
#                      breaks = c(0.32, seq(0.35, 0.6, 0.05),0.65)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(title = "IV: Share of female representatives",
#        x = "\nShare",
#        y = "Predicted proportions\n") +
#   theme_fdtsr


## Model 22 ----

# Adding the share of occasions that the governing body was chaired by a female legislator in the corresponding legislative period (shr_female_chair_gb) as an explanatory variable 

m22 <- 
  glmer(cbind(bills_pass, bills_pass_others) ~ 1 + shr_female_legislators + shr_female_chair_gb + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m22)
fixef(m22) # Fixed effects
ranef(m22) # Random effects


# ggpredict(m22, 
#           terms = c("shr_female_dhair_gb [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-0.01, 0.76),
#                      breaks = seq(0, 0.9, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.025)) +
#   labs(title = "IV: Share of occasions that the governing body was chaired by a female legislator in the\ncorresponding legislative period",
#        
#        x = "\nShare",
#        y = "Predicted proportions\n") +
#   theme_fdtsr


## Model 23 ----

# Adding the share of the governing body members that were female legislators (shr_female_members_gb) as an explanatory variable 

m23 <- 
  glmer(cbind(bills_pass, bills_pass_others) ~ 1 + shr_female_legislators + shr_female_members_gb + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m23)
fixef(m23) # Fixed effects
ranef(m23) ## Random effects


# ggpredict(m23, 
#           terms = c("shr_female_members_gb [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0.15, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Share of the governing body members that were female legislators",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr



## Model 24 ----

# Adding the share of committees that were chaired by a female legislator in the corresponding legislative period (shr_female_chair_committees) as an explanatory variable 

m24 <- 
  glmer(cbind(bills_pass, bills_pass_others) ~ 1 + shr_female_legislators + shr_female_chair_committees + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m24)
fixef(m24) # Fixed effects
ranef(m24) ## Random effects


# ggpredict(m24, 
#           terms = c("shr_female_chair_committees [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0.15, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Share of committees that were chaired by a female legislator in the corresponding legislative period",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr


## Model 25 ----

# Adding the mean share of the committee members that were female legislators (shr_female_members_committees) as an explanatory variable 

m25 <- 
  glmer(cbind(bills_pass, bills_pass_others) ~ 1 + shr_female_legislators + shr_female_members_committees + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m25)
fixef(m25) # Efectos fijos
ranef(m25) # Efectos aleatorios


# ggpredict(m25, 
#           terms = c("shr_female_members_committees [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Mean share of the committee members that were female legislators",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr


### Models 26 to 30 ----

# In these specifications:

# 1) The response variable is the share of feminist bills presented by female legislators that were approved with respect to the total number of feminist bills approved, defined in every case as cbind(bills_fem_pass, bills_fem_others_pass). This is so because when using cbind() inside glm() with family = binomial(link = "logit"), R expects the first figure to be the number of successes and the second the number of failures. Here bills_fem_others_pass is the number of feminist bills presented by non-female legislators that were approved (i.e., the number of failures).
# 2) Observations are nested by state and legislative period, specified in every case as (1 | state/leg_period)
# 3) Fixed and random effects are estimated
# 4) In order to estimate the fixed effects, the following explanatory variables are included: i) shr_female_legislators, ii) state_gpd_rescaled, iii) percent_urban_pop, iv) avg_yr_school 
# 5) In models 2 to 5 we also include one of the following explanatory variables that measure symbolic representation


## Model 26 ----
m26 <- 
  glmer(cbind(bills_fem_pass, bills_fem_others_pass) ~ 1 + shr_female_legislators + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m26)
fixef(m26) # Fixed effects
ranef(m26) # Random effects

# ggpredict(m26, 
#           terms = c("shr_female_legislators [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>%  
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(0.31, 0.66),
#                      breaks = c(0.32, seq(0.35, 0.6, 0.05),0.65)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(title = "IV: Share of female representatives",
#        x = "\nShare",
#        y = "Predicted proportions\n") +
#   theme_fdtsr

## Model 27 ----

# Adding the share of occasions that the governing body was chaired by a female legislator in the corresponding legislative period (shr_female_chair_gb) as an explanatory variable 

m27 <- 
  glmer(cbind(bills_fem_pass, bills_fem_others_pass) ~ 1 + shr_female_legislators + shr_female_chair_gb + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m27)
fixef(m27) # Fixed effects
ranef(m27) # Random effects


# ggpredict(m27, 
#           terms = c("shr_female_dhair_gb [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      limits = c(-0.01, 0.76),
#                      breaks = seq(0, 0.9, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.025)) +
#   labs(title = "IV: Share of occasions that the governing body was chaired by a female legislator in the\ncorresponding legislative period",
#        
#        x = "\nShare",
#        y = "Predicted proportions\n") +
#   theme_fdtsr


## Model 28 ----

# Adding the share of the governing body members that were female legislators (shr_female_members_gb) as an explanatory variable 

m28 <- 
  glmer(cbind(bills_fem_pass, bills_fem_others_pass) ~ 1 + shr_female_legislators + shr_female_members_gb + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m28)
fixef(m28) # Fixed effects
ranef(m28) ## Random effects


# ggpredict(m28, 
#           terms = c("shr_female_members_gb [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0.15, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Share of the governing body members that were female legislators",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr



## Model 29 ----

# Adding the share of committees that were chaired by a female legislator in the corresponding legislative period (shr_female_chair_committees) as an explanatory variable 

m29 <- 
  glmer(cbind(bills_fem_pass, bills_fem_others_pass) ~ 1 + shr_female_legislators + shr_female_chair_committees + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m29)
fixef(m29) # Fixed effects
ranef(m29) ## Random effects


# ggpredict(m29, 
#           terms = c("shr_female_chair_committees [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0.15, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Share of committees that were chaired by a female legislator in the corresponding legislative period",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr


## Model 30 ----

# Adding the mean share of the committee members that were female legislators (shr_female_members_committees) as an explanatory variable 

m30 <- 
  glmer(cbind(bills_fem_pass, bills_fem_others_pass) ~ 1 + shr_female_legislators + shr_female_members_committees + state_gpd_rescaled + percent_urban_pop + avg_yr_school + (1 | state) + (1 | leg_period), 
        data = bd_rep_sus_agg_fem, 
        family = binomial(link = "logit"))

summary(m30)
fixef(m30) # Efectos fijos
ranef(m30) # Efectos aleatorios


# ggpredict(m30, 
#           terms = c("shr_female_members_committees [all]"), 
#           type = "fixed", nsim = 5000) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x = x, 
#              y = predicted)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
#   geom_line() +
#   scale_x_continuous(expand = c(0, 0),
#                      breaks = seq(0, 0.6, 0.05)) +
#   scale_y_continuous(breaks = seq(0, 0.9, 0.05)) +
#   labs(
#     title = "IV: Mean share of the committee members that were female legislators",
#     x = "\nShare",
#     y = "Predicted proportions\n") +
#   theme_fdtsr


### Tables ----

## Models 1 to 5 ----
stargazer(m1, m2, m3, m4, m5, 
          title = "Results", align = TRUE, 
          type = "text", dep.var.caption = "Share of bills presented by female legislators with respect to the total number of bills presented by all legislators", 
          out = "04_generated_data/ch_5/models/ch_5_results_table_models_1_5_20211110.txt")


## Models 6 to 10 ----
stargazer(m6, m7, m8, m9, m10, 
          title = "Results", align = TRUE, 
          type = "text", dep.var.caption = "Share of feminist bills presented by female legislators with respect to the total number of feminist bills presented by all legislators", 
          out = "04_generated_data/ch_5/models/ch_5_results_table_models_6_10_20211110.txt")


## Models 11 to 15 ----
stargazer(m11, m12, m13, m14, m15, 
          title = "Results", align = TRUE, 
          type = "text", dep.var.caption = "Share of bills presented by female legislators that were approved with respect to the total number of bills presented by female legislators", 
          out = "04_generated_data/ch_5/models/ch_5_results_table_models_11_15_20211110.txt")

## Models 16 to 20 ----
stargazer(m16, m17, m18, m19, m20, 
          title = "Results", align = TRUE, 
          type = "text", dep.var.caption = "Share of feminist bills presented by female legislators that were approved with respect to the total number of feminist bills presented by female legislators", 
          out = "04_generated_data/ch_5/models/ch_5_results_table_models_16_20_20211110.txt")

## Models 21 to 25 ----
stargazer(m21, m22, m23, m24, m25, 
          title = "Results", align = TRUE, 
          type = "text", dep.var.caption = "Share of bills presented by female legislators that were approved with respect to the total number of bills approved", 
          out = "04_generated_data/ch_5/models/ch_5_results_table_models_21_25_20211110.txt")


## Models 26 to 30 ----
stargazer(m26, m27, m28, m29, m30, 
          title = "Results", align = TRUE, 
          type = "text", dep.var.caption = "Share of feminist bills presented by female legislators that were approved with respect to the total number of feminist bills approved", 
          out = "04_generated_data/ch_5/models/ch_5_results_table_models_26_30_20211110.txt")


### Figures ----

## Figure 5.5 ----

# The predictions included in these graphs where calculated using the corresponding fixed effects coefficients of model 2

t1_g1 <- 
  ggpredict(m2,
            terms = c("shr_female_legislators [all]"),
            type = "fixed", nsim = 5000) %>%
  as_tibble() %>%
  # print(n = Inf)
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0.31, 0.66),
                     breaks = seq(0, 0.9, 0.05)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1),
                     limits = c(0.14, 0.72)) +
  labs(title = "Share of female representatives",
       
       x = "\nShare",
       y = "Predicted proportions\n") +
  theme_fdtsr_predictions

t1_g2 <- 
  ggpredict(m2,
            terms = c("shr_female_chair_gb [all]"),
            type = "fixed", nsim = 5000) %>%
  as_tibble() %>%
  # print(n = Inf)
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-0.01, 0.76),
                     breaks = seq(0, 0.9, 0.1)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1),
                     limits = c(0.14, 0.72)) +
  labs(title = "Share of female presidents of the governing body",
       
       x = "\nShare",
       y = NULL) +
  theme_fdtsr_predictions

t1_g3 <- 
  ggpredict(m2,
            terms = c("percent_urban_pop [all]"),
            type = "fixed", nsim = 5000) %>%
  as_tibble() %>%
  # print(n = Inf)
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0),
                     # limits = c(0.180, 0.376),
                     breaks = seq(50, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1),
                     limits = c(0.14, 0.72)) +
  labs(title = "Urbanization",
       
       x = "\nPercent urban population",
       y = "Predicted proportions\n") +
  theme_fdtsr_predictions

t1_g4 <- 
  ggpredict(m2,
            terms = c("avg_yr_school [all]"),
            type = "fixed", nsim = 5000) %>%
  as_tibble() %>%
  # print(n = Inf)
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(7.2, 11.6),
                     breaks = seq(7, 13, 0.5)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1),
                     limits = c(0.14, 0.72)) +
  labs(title = "Average years of school",
       
       x = "\nAverage years",
       y = NULL) +
  theme_fdtsr_predictions

plot_grid(t1_g1, t1_g2, t1_g3, t1_g4, ncol = 2)

ggsave("03_vis/ch_5/figure_5.5_predictions_fe_model_2.png", width = 14, height = 9, dpi = 200)
ggsave("03_vis/ch_5/figure_5.5_predictions_fe_model_2.tiff", width = 14, height = 9, dpi = 200)

## Figure 5.6 ----

# The predictions included in these graphs where calculated using the corresponding fixed effects coefficients of models 14 and 16

t2_g1 <- 
  ggpredict(m14,
            terms = c("shr_female_legislators [all]"),
            type = "fixed", nsim = 5000) %>%
  as_tibble() %>%
  # print(n = Inf)
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0.31, 0.66),
                     breaks = seq(0, 0.9, 0.05)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1),
                     limits = c(0.01, 0.86)) +
  labs(title = "Model 14 - Share of female representatives\n",
       
       x = "\nShare",
       y = "Predicted proportions\n") +
  theme_fdtsr_predictions

t2_g2 <- 
  ggpredict(m14,
            terms = c("shr_female_chair_committees [all]"),
            type = "fixed", nsim = 5000) %>%
  as_tibble() %>%
  # print(n = Inf)
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-0.01, 0.87),
                     breaks = seq(0, 0.9, 0.1)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1),
                     limits = c(0.01, 0.86)) +
  labs(title = "Model 14 - Share of committees chaired by a female\nlegislator",
       
       x = "\nShare",
       y = NULL) +
  theme_fdtsr_predictions

t2_g3 <- 
  ggpredict(m14,
            terms = c("avg_yr_school [all]"),
            type = "fixed", nsim = 5000) %>%
  as_tibble() %>%
  # print(n = Inf)
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(7.2, 11.6),
                     breaks = seq(7, 13, 0.5)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1),
                     limits = c(0.01, 0.86)) +
  labs(title = "Model 14 - Average years of school\n",
       
       x = "\nAverage years",
       y = "Predicted proportions\n") +
  theme_fdtsr_predictions


t2_g4 <- 
  ggpredict(m16,
            terms = c("shr_female_legislators [all]"),
            type = "fixed", nsim = 5000) %>%
  as_tibble() %>%
  # print(n = Inf)
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0.31, 0.66),
                     breaks = seq(0, 0.9, 0.05)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1),
                     limits = c(0.01, 0.86)) +
  labs(title = "Model 16 - Share of female representatives\n",
       
       x = "\nShare",
       y = NULL) +
  theme_fdtsr_predictions

plot_grid(t2_g1, t2_g2, t2_g3, t2_g4, ncol = 2)

ggsave("03_vis/ch_5/figure_5.6_predictions_fe_models_14_and_16.png", width = 14, height = 9, dpi = 200)
ggsave("03_vis/ch_5/figure_5.6_predictions_fe_models_14_and_16.tiff", width = 14, height = 9, dpi = 200)

## Figure 5.7 ----

# The predictions included in these graphs where calculated using the corresponding fixed effects coefficients of models 22 and 27

t3_g1 <- 
  ggpredict(m22,
            terms = c("shr_female_legislators [all]"),
            type = "fixed", nsim = 5000) %>%
  as_tibble() %>%
  # print(n = Inf)
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(0.31, 0.66),
                     breaks = seq(0, 0.9, 0.05)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1),
                     limits = c(0.1, 0.71)) +
  labs(title = "Model 22 - Share of female representatives",
       
       x = "\nShare",
       y = "Predicted proportions\n") +
  theme_fdtsr_predictions

t3_g2 <- 
  ggpredict(m22,
            terms = c("shr_female_chair_gb [all]"),
            type = "fixed", nsim = 5000) %>%
  as_tibble() %>%
  # print(n = Inf)
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-0.01, 0.76),
                     breaks = seq(0, 0.9, 0.1)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1),
                     limits = c(0.1, 0.71)) +
  labs(title = "Model 22 - Share of female presidents of the governing body",
       
       x = "\nShare",
       y = NULL) +
  theme_fdtsr_predictions


t3_g3 <- 
  ggpredict(m22,
            terms = c("percent_urban_pop [all]"),
            type = "fixed", nsim = 5000) %>%
  as_tibble() %>%
  # print(n = Inf)
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0),
                     # limits = c(0.180, 0.376),
                     breaks = seq(50, 100, 5)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1),
                     limits = c(0.1, 0.71)) +
  labs(title = "Model 22 - Urbanization",
       
       x = "\nPercent urban population",
       y = "Predicted proportions\n") +
  theme_fdtsr_predictions


t3_g4 <- 
  ggpredict(m27,
            terms = c("shr_female_chair_gb [all]"),
            type = "fixed", nsim = 5000) %>%
  as_tibble() %>%
  # print(n = Inf)
  ggplot(aes(x = x,
             y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(-0.01, 0.76),
                     breaks = seq(0, 0.9, 0.1)) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1),
                     limits = c(0.1, 0.71)) +
  labs(title = "Model 27 - Share of female presidents of the governing body",
       
       x = "\nShare",
       y = NULL) +
  theme_fdtsr_predictions

plot_grid(t3_g1, t3_g2, t3_g3, t3_g4, ncol = 2)

ggsave("03_vis/ch_5/figure_5.7_predictions_fe_models_22_and_27.png", width = 16, height = 9, dpi = 200)
ggsave("03_vis/ch_5/figure_5.7_predictions_fe_models_22_and_27.tiff", width = 16, height = 9, dpi = 200)

