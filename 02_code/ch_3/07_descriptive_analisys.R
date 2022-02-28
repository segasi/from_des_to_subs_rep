### Load packages, define setup and vis theme ----
source("02_code/ch_3/00_pacakges_setup_theme.R")

### Import, process and join data bases ----
source("02_code/ch_3/06_join_data.R")

### Total number of legislative periods analyzed, by electoral rule ----

# Majority rule
bd_mr %>% 
  tally()

# Proportional representation
bd_pr %>% 
  tally()

### Figure 3.1: Number of legislative periods analyzed by state ----

# Data
bd_mr %>% 
  count(start_yr) %>% 
  print(n = Inf)

# Graph
bd_mr %>% 
  count(state, sort = T) %>% 
  ggplot(aes(x = n, y = fct_reorder(state, n))) +
  geom_segment(aes(x = 0, xend = n, y = fct_reorder(state, n), yend = fct_reorder(state, n)),
               size = 1, color = "grey40") +
  geom_point(size = 3, color = "grey40") +
  theme_fdtsr + 
  scale_x_continuous(breaks = 0:12, expand = c(0, 0),
                     limits = c(-0.1, 11.1)) +
  # Spanish version
  # labs(x = "\nNúmero de legislaturas estatales incluidas en el análisis",
  #      y = NULL) +
  # English version
  labs(x = "\nNumber of legislative periods analyzed by state",
       y = NULL) +
  theme(panel.grid.major.y = element_blank()) 

ggsave("03_vis/ch_3/figure_3.1_number_of_legislative_periods_analyzed_by_state_1987_2021.png", width = 16, height = 9, dpi = 200)
ggsave("03_vis/ch_3/figure_3.1_number_of_legislative_periods_analyzed_by_state_1987_2021.tiff", width = 16, height = 9, dpi = 200)


### Number of changes in gender electoral laws, by year ----

# Majority rule
bd_index_state %>% 
  filter(str_detect(electoral_rule, "Major")) %>% 
  count(state, law_publication_year) %>% 
  count(law_publication_year) %>%
  mutate(total = sum(n)) %>% 
  print(n = Inf)

# Proportional representation 
bd_index_state %>% 
  filter(str_detect(electoral_rule, "Prop")) %>% 
  count(state, law_publication_year) %>% 
  count(law_publication_year) %>%
  mutate(total = sum(n)) %>% 
  print(n = Inf)



### Number of changes in gender electoral laws, by state ----

# Majority rule
bd_index_state %>% 
  filter(str_detect(electoral_rule, "Major")) %>% 
  count(state, law_publication_year) %>%
  count(state) %>%
  mutate(total = sum(n)) %>% 
  print(n = Inf)

# Proportional representation 
bd_index_state %>% 
  filter(!str_detect(electoral_rule, "Rep")) %>% 
  count(state, law_publication_year) %>%
  count(state) %>%
  mutate(total = sum(n)) %>% 
  print(n = Inf)


### Figure 3.2: Mean percentage of female and male state legislators elected by either majority rule or proportional representation, 1991-2019---- 

# Data
bd_leg_all %>% 
  # Include a couple of fake observations in order to assure that in the next step all states have values for 2014 and 2020
  add_row(state = "foo", start_yr = c(2014)) %>% 
  add_row(state = "foo", start_yr = c(2020)) %>% 
  complete(state, nesting(start_yr)) %>% 
  # Eliminate fakes observations of state "foo"
  filter(state != "foo") %>% 
  # Replace NAs by the value of the previous cell, by state
  group_by(state) %>% 
  fill(c(period, n_female_legislators, n_male_legislators, total_legislators, prop_female_legislators, prop_male_legislators), .direction = "down") %>%
  ungroup() %>% 
  filter(!is.na(period)) %>% 
  group_by(start_yr) %>% 
  summarise(`Female legislators` = mean(prop_female_legislators*100),
            `Male legislators` = mean(prop_male_legislators*100)) %>% 
  ungroup() %>% 
  filter(start_yr > 1990, 
         start_yr < 2020) %>% 
  mutate(gap = `Male legislators` - `Female legislators`) %>% 
  print(n = Inf)
  

# Vis
bd_leg_all %>% 
  # Include a couple of fake observations in order to assure that in the next step all states have values for 2014 and 2020
  add_row(state = "foo", start_yr = c(2014)) %>% 
  add_row(state = "foo", start_yr = c(2020)) %>% 
  complete(state, nesting(start_yr)) %>% 
  # Eliminate fakes observations of state "foo"
  filter(state != "foo") %>% 
  # Replace NAs by the value of the previous cell, by state
  group_by(state) %>% 
  fill(c(period, n_female_legislators, n_male_legislators, total_legislators, prop_female_legislators, prop_male_legislators), .direction = "down") %>%
  ungroup() %>% 
  filter(!is.na(period)) %>% 
  group_by(start_yr) %>% 
  summarise(`Female legislators` = mean(prop_female_legislators*100),
            `Male legislators` = mean(prop_male_legislators*100)) %>% 
  ungroup() %>% 
  filter(start_yr > 1990, 
         start_yr < 2020) %>% 
  pivot_longer(-start_yr,
               names_to = "type",
               values_to = "percent") %>% 
  ggplot(aes(x = start_yr, 
             y = percent,
             color = type,
             shape = type)) +
  geom_point(size = 5) +
  scale_x_continuous(breaks = c(1987, seq(1991, 2020, 1)), 
                     limits = c(1990.5, 2019.5),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  # Spanish version
  # scale_shape_manual(values = c(`Male legislators` = 21, `Female legislators` = 19),
  #                    labels = c(`Male legislators` = "Legisladores",  `Female legislators` = "Legisladoras")) +
  # scale_color_manual(values = c(`Male legislators` = "grey40", `Female legislators` = "grey40"),
  #                    labels = c(`Male legislators` = "Legisladores",  `Female legislators` = "Legisladoras")) +
  # English version
  scale_shape_manual(values = c(`Male legislators` = 21, `Female legislators` = 19),
                     labels = c(`Male legislators` = "Male legislators",  `Female legislators` = "Female legislators")) +
  scale_color_manual(values = c(`Male legislators` = "grey40", `Female legislators` = "grey40"),
                     labels = c(Legisladores = "Male legislators",  `Female legislators` = "Female legislators")) +
  # Spanish version
  # labs(x = "\nAño   ",
  #      y = "Promedio porcentual   \n",
  #      color = NULL,
  #      shape = NULL) +
  # English version
  labs(x = "\nYear   ",
       y = "Percentage average   \n",
       color = NULL,
       shape = NULL) +
  theme_fdtsr + 
  theme(axis.text = element_text(size = 23),
        axis.text.x = element_text(size = 23, angle = 90, hjust = 1, vjust = 0.5),
        axis.title = element_text(size = 26),
        legend.text = element_text(size = 20, lineheight = 1.1),
        legend.key.height = unit(1, "cm"),
        legend.direction = "vertical",
        legend.position = c(0.88, 0.87)) 
  
ggsave("03_vis/ch_3/figure_3.2_mean_ percentage_female_male_state_legislators_1991_2019.png", width = 16, height = 9, dpi = 200)
ggsave("03_vis/ch_3/figure_3.2_mean_ percentage_female_male_state_legislators_1991_2019.tiff", width = 16, height = 9, dpi = 200)

### Figure 3.3: Percentage of female state legislators elected by either majority rule or proportional representation, 1987-2019 ----

bd_leg_all %>% 
  ggplot(aes(x = start_yr, 
             y = prop_female_legislators*100,
             group = state)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 50, color = "salmon", linetype = 3) +
  scale_x_continuous(breaks = c(seq(1990, 2015, 5), 2019)) +
  scale_y_continuous(breaks = seq(0, 70, 10)) +
  facet_wrap(~ state, ncol= 8) +
  # Spanish version
  # labs(title = NULL, 
       # x = "\n",
       # y = "Porcentaje\n") +
  # English version
  labs(title = NULL,
       x = "\n",
       y = "Percentage\n") +
  theme_fdtsr +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 12),
        axis.text.y = element_text(size = 11),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2))

ggsave("03_vis/ch_3/figure_3.3_percentage_female_state_legislators_1987_2019.png", width = 16, height = 9, dpi = 200)
ggsave("03_vis/ch_3/figure_3.3_percentage_female_state_legislators_1987_2019.tiff", width = 16, height = 9, dpi = 200)





### Figure 3.4: Evolution of the Electoral Gender Regime Strength Index ---- 
bd_index_state %>% 
  select(state, law_publication_year, electoral_rule, ifreg) %>% 
  distinct() %>% 
  # print(n = Inf)
  ggplot(aes(x = law_publication_year, 
             y = ifreg, 
             color = electoral_rule, 
             group = electoral_rule)) +
  geom_vline(xintercept = 2014, color = "salmon", linetype = 2, size = 1) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  scale_color_manual(values = c("grey70", "grey30")) +
  facet_wrap(~ state, ncol= 8) +
  # Spanish version
  # labs(title = NULL, 
  #      x = "\nAño de publicación\n",
  #      y = "Valor\n", 
  #      color = NULL) +
  # English version
  labs(title = NULL,
       x = "\nYear of publication\n",
       y = "Value\n",
       color = NULL) +
  theme_fdtsr +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 13),
        axis.text.y = element_text(size = 13),
        panel.border = element_rect(colour = "grey70", fill = "transparent", size = 0.2),
        legend.position = c(0.16, -0.11),
        legend.direction = "horizontal", 
        legend.text = element_text(size = 17))

ggsave("03_vis/ch_3/figure_3.4_evolution_of_egrsi_in_each_mexican state_1994_2016.png", width = 16, height = 9, dpi = 200)
ggsave("03_vis/ch_3/figure_3.4_evolution_of_egrsi_in_each_mexican state_1994_2016.tiff", width = 16, height = 9, dpi = 200)



### Exploratory analysis of the response and explanatory variables  ----

## Proportion of female legislators ----

# Five-number summary of the proportion of female legislators elected by MR and PR
summary(bd_mr$prop_female_legislators)
summary(bd_pr$prop_female_legislators)

# Standard deviation of the proportion of female legislators elected by MR and PR
sd(bd_mr$prop_female_legislators) 
sd(bd_pr$prop_female_legislators)


# Rango de medias y Standard deviation estatales of the proportion of female legislators elected by MR
bd_mr %>% 
  group_by(state) %>% 
  summarise(avg_prop_female_legislators = mean(prop_female_legislators),
            sd_prop_female_legislators = sd(prop_female_legislators)) %>% 
  ungroup() %>% 
  arrange(-sd_prop_female_legislators) %>%
  print(n = Inf)


# Rango de medias y Standard deviation estatales of the proportion of female legislators elected by PR
bd_pr %>% 
  group_by(state) %>% 
  summarise(avg_prop_female_legislators = mean(prop_female_legislators),
            sd_prop_female_legislators = sd(prop_female_legislators)) %>% 
  ungroup() %>% 
  arrange(-sd_prop_female_legislators) %>%
  print(n = Inf)


## IFREG ----

# Five-number summary del IFREG para MR y RP
summary(bd_mr$ifreg)
summary(bd_pr$ifreg)

# Standard deviation del IFREG para MR y RP
sd(bd_mr$ifreg) 
sd(bd_pr$ifreg)


# Rango de medias y Standard deviation estatales del IFREG para MR
bd_mr %>% 
  group_by(state) %>% 
  summarise(avg_ifreg = mean(ifreg),
            sd_ifreg = sd(ifreg)) %>% 
  ungroup() %>% 
  arrange(-avg_ifreg) %>%
  print(n = Inf)


# Rango de medias y Standard deviation estatales del IFREG para RP
bd_pr %>% 
  group_by(state) %>% 
  summarise(avg_ifreg = mean(ifreg),
            sd_ifreg = sd(ifreg)) %>% 
  ungroup() %>% 
  arrange(-avg_ifreg) %>%
  print(n = Inf)

## Antigüedad norma de género ----
 
# Five-number summary para MR y RP
summary(bd_mr$age_first_gender_law)
summary(bd_pr$age_first_gender_law)

# Standard deviation for MR and PR
sd(bd_mr$age_first_gender_law, na.rm = T) 
sd(bd_pr$age_first_gender_law, na.rm = T)


# Rango de medias y Standard deviation estatales de antigüedad norma de género para MR
bd_pr %>% 
  group_by(state) %>% 
  summarise(avg_age_first_gender_law = mean(age_first_gender_law, na.rm = T),
            sd_age_first_gender_law = sd(age_first_gender_law, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(-avg_age_first_gender_law) %>%
  # arrange(-des_est_age_first_gender_law) %>%
  print(n = Inf)


## PIB estatal per cápita ----

# Five-number summary
summary(bd_mr$state_gpd_per_capita)

# Standard deviation for MR and PR
sd(bd_mr$state_gpd_per_capita, na.rm = T) 
sd(bd_pr$state_gpd_per_capita, na.rm = T)


# Range of state means and standard deviations for MR
bd_mr %>% 
  group_by(state) %>% 
  summarise(avg_state_gpd_per_capita = mean(state_gpd_per_capita, na.rm = T),
            sd_gpd_per_capita = sd(state_gpd_per_capita, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(-avg_state_gpd_per_capita) %>%
  # arrange(-sd_gpd_per_capita) %>%
  print(n = Inf)

## Party alternation ----

# Five-number summary
summary(bd_mr$party_alternation)
summary(bd_pr$party_alternation)

# Standard deviation for MR and PR
sd(bd_mr$party_alternation, na.rm = T) 
sd(bd_pr$party_alternation, na.rm = T)


# Range of state means and standard deviations for MR
bd_mr %>% 
  group_by(state) %>% 
  summarise(avg_party_alternation = mean(party_alternation, na.rm = T),
            des_party_alternation = sd(party_alternation, na.rm = T)) %>% 
  ungroup() %>% 
  arrange(-avg_party_alternation) %>%
  # arrange(-des_party_alternation) %>%
  print(n = Inf)
  
## Percentage of urban population ----

# Five-number summary
summary(bd_mr$percent_urban_pop)

# Standard deviation for MR and PR
sd(bd_mr$percent_urban_pop, na.rm = T) 
sd(bd_pr$percent_urban_pop, na.rm = T)


# Range of state means and standard deviations for MR
bd_mr %>% 
  group_by(state) %>% 
  summarise(avg_percent_urban_pop = mean(percent_urban_pop, na.rm = T),
            des_percent_urban_pop = sd(percent_urban_pop, na.rm = T)) %>% 
  ungroup() %>% 
  # arrange(-avg_percent_urban_pop) %>%
  arrange(-des_percent_urban_pop) %>%
  print(n = Inf)


## Average years of education ----

# Five-number summary
summary(bd_mr$avg_yr_school)

# Standard deviation for MR and PR
sd(bd_mr$avg_yr_school, na.rm = T) 
sd(bd_pr$avg_yr_school, na.rm = T)


# Range of state means and standard deviations for MR
bd_mr %>% 
  group_by(state) %>% 
  summarise(avg_avg_yr_school = mean(avg_yr_school, na.rm = T),
            des_avg_yr_school = sd(avg_yr_school, na.rm = T)) %>% 
  ungroup() %>% 
  # arrange(-avg_avg_yr_school) %>%
  arrange(-des_avg_yr_school) %>%
  print(n = Inf)


### Analysis of correlation between IFREG y age of first gender law ----

# All observations of bd_mr ----
bd_mr %>% 
  filter(!is.na(age_first_gender_law)) %>% 
  summarise(correlacion_pearson = cor(age_first_gender_law, ifreg, method = "pearson"),
            correlacion_spearman = cor(age_first_gender_law, ifreg, method = "spearman"),
            correlacion_kendall = cor(age_first_gender_law, ifreg, method = "kendall")) 

# By state, with data from bd_mr ----
bd_mr %>% 
  filter(!is.na(age_first_gender_law)) %>% 
  group_by(state) %>% 
  summarise(correlacion_pearson = cor(age_first_gender_law, ifreg, method = "pearson"),
            correlacion_spearman = cor(age_first_gender_law, ifreg, method = "spearman"),
            correlacion_kendall = cor(age_first_gender_law, ifreg, method = "kendall")) %>% 
  ungroup() %>% 
  arrange(-correlacion_spearman) %>% 
  print(n = Inf)


# All observations of bd_pr ----
bd_pr %>% 
  filter(!is.na(age_first_gender_law)) %>% 
  summarise(correlacion_pearson = cor(age_first_gender_law, ifreg, method = "pearson"),
            correlacion_spearman = cor(age_first_gender_law, ifreg, method = "spearman"),
            correlacion_kendall = cor(age_first_gender_law, ifreg, method = "kendall")) 

# By state, with data from bd_pr ----
bd_pr %>% 
  filter(!is.na(age_first_gender_law)) %>% 
  group_by(state) %>% 
  summarise(correlacion_pearson = cor(age_first_gender_law, ifreg, method = "pearson"),
            correlacion_spearman = cor(age_first_gender_law, ifreg, method = "spearman"),
            correlacion_kendall = cor(age_first_gender_law, ifreg, method = "kendall")) %>% 
  ungroup() %>% 
  arrange(-correlacion_spearman) %>% 
  print(n = Inf)



### Analysis of correlation between GDP per capita, percentage of urban population and average years of education ----

# All observations of bd_mr ----
foo <- 
  bd_mr %>% 
  select(state_gpd_per_capita_rescaled, percent_urban_pop, avg_yr_school)


# Pearson
corrplot::corrplot(cor(foo, use = "complete.obs", method = "pearson"), type = "upper", method = "number")

# Spearman
corrplot::corrplot(cor(foo, use = "complete.obs", method = "spearman"), type = "upper", method = "number")

# Kendall
corrplot::corrplot(cor(foo, use = "complete.obs", method = "kendall"), type = "upper", method = "number")


# All observations of bd_pr ----
faa <- 
  bd_pr %>% 
  select(state_gpd_per_capita_rescaled, percent_urban_pop, avg_yr_school)


# Pearson
corrplot::corrplot(cor(faa, use = "complete.obs", method = "pearson"), type = "upper", method = "number")

# Spearman
corrplot::corrplot(cor(faa, use = "complete.obs", method = "spearman"), type = "upper", method = "number")

# Kendall
corrplot::corrplot(cor(faa, use = "complete.obs", method = "kendall"), type = "upper", method = "number")


### Evolution of the number of legislators elected by each principle ----

# Vis not included un the chapter
bd_mr %>% 
  ggplot(aes(x = start_yr, y = total_legislators, group = state)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 40, 5)) +
  facet_wrap(~ state, ncol = 8)

bd_pr %>% 
  ggplot(aes(x = start_yr, y = total_legislators, group = state)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = seq(0, 40, 5)) +
  facet_wrap(~ state, ncol = 8)


### Evolution of the female legislators elected in each state and legislative period by each principle ----
bd_mr %>% 
  left_join(bd_pr %>% 
              rename(prop_female_legislators_pr = prop_female_legislators) %>% 
              select(state, period, prop_female_legislators_pr),
            by = c("state", "period")) %>% 
  ggplot() +
  geom_line(aes(x = start_yr, y = prop_female_legislators)) +
  geom_point(aes(x = start_yr, y = prop_female_legislators)) +
  geom_line(aes(x = start_yr, y = prop_female_legislators_pr), color = "salmon") +
  geom_point(aes(x = start_yr, y = prop_female_legislators_pr), color = "salmon") +
  facet_wrap(~ state, ncol = 8) +
  labs(title = "MR = black | PR = red")
