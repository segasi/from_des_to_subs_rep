### Clean working environment ----
remove(list = ls())

### Run 03_join_data script.R ----

# This will load packages, define setup and vis themes, import and prepare data
source("02_code/ch_5/03_join_data.R")

### Bills presented, general statistics ----

# Total number of bills analyzed
bd_rep_sus %>% 
  summarise(n_bills = n())

# Bills presented by state
foo <- 
  bd_rep_sus %>% 
  count(state, sort = T) 

foo %>% 
  print(n = Inf)

summary(foo$n)
sd(foo$n)

# Bills presented by period
bd_rep_sus %>% 
  group_by(leg_period) %>% 
  summarise(n_bills = n()) %>% 
  ungroup() %>% 
  mutate(per = n_bills/sum(n_bills)*100)

# Bills presented by state and period
bd_rep_sus %>% 
  group_by(leg_period, state) %>% 
  summarise(n_bills = n()) %>% 
  ungroup() %>% 
  arrange(state, leg_period) %>% 
  group_by(state) %>% 
  mutate(cambio = (n_bills - lag(n_bills))/lag(n_bills)*100) %>% 
  ungroup() %>% 
  filter(!is.na(cambio)) %>% 
  arrange(-cambio) %>% 
  print(n = Inf)

# Bills by status 
bd_rep_sus %>% 
  count(status) %>% 
  mutate(total = sum(n), 
         por = n/sum(n)*100)

# Bills by period and status 
bd_rep_sus %>% 
  group_by(leg_period, status) %>% 
  summarise(n_bills = n()) %>% 
  ungroup() %>% 
  group_by(leg_period) %>% 
  mutate(por = n_bills/sum(n_bills)*100) %>% 
  ungroup()

# Bills by period, status and gender_gral
bd_rep_sus %>% 
  group_by(leg_period, status, gender_gral) %>% 
  summarise(n_bills = n()) %>% 
  ungroup() %>% 
  group_by(leg_period, gender_gral) %>% 
  mutate(por = n_bills/sum(n_bills)*100) %>% 
  ungroup() %>% 
  arrange(gender_gral, status)


# Bills presented by gender_gral

# Important: gender_gral groups the bills presented by individual female legislators and groups of female legislators in the category Female, as well as those presented by individual male legislators and groups of male legislators in the category Male

bd_rep_sus %>% 
  count(gender_gral) %>% 
  mutate(per = n/sum(n)*100)

# Bills presented by gender_gral and period
bd_rep_sus %>% 
  group_by(leg_period, gender_gral) %>% 
  summarise(n_bills = n()) %>% 
  ungroup() %>% 
  group_by(leg_period) %>% 
  mutate(por = n_bills/sum(n_bills)*100) %>% 
  ungroup()

# Change in bills presented by gender_gral across periods
bd_rep_sus %>% 
  group_by(leg_period, gender_gral) %>% 
  summarise(n_bills = n()) %>% 
  ungroup() %>% 
  arrange(gender_gral, leg_period) %>% 
  group_by(gender_gral) %>% 
  mutate(cambio = (n_bills - lag(n_bills))/lag(n_bills)*100) %>% 
  ungroup() %>% 
  as.data.frame()

# Feminist bills
bd_rep_sus %>% 
  count(feminist) %>% 
  mutate(por = n/sum(n)*100)

# Feminist bills by period
bd_rep_sus %>% 
  group_by(leg_period, feminist) %>% 
  summarise(n_bills = n()) %>% 
  ungroup() %>% 
  group_by(leg_period) %>% 
  mutate(por = n_bills/sum(n_bills)*100) %>% 
  ungroup()

# Feminist bills by gender
bd_rep_sus %>% 
  filter(feminist == "Yes") %>% 
  count(gender_gral) %>% 
  mutate(por = n/sum(n)*100)

# Feminist bills by status
bd_rep_sus %>% 
  filter(feminist == "Yes") %>% 
  count(status) %>% 
  mutate(por = n/sum(n)*100)

### Bills presented by female legislators ----

# General bills presented by female legislators
bd_rep_sus %>% 
  summarise(n = sum(gender_gral == "Female"))

# Feminist bills presented by female legislators
bd_rep_sus %>% 
  summarise(n = sum(gender_gral == "Female" & feminist == "Yes"))

# General bills presented by female legislators
bd_rep_sus %>% 
  group_by(state) %>% 
  summarise(n_female = sum(gender_gral == "Female"),
            total = n()) %>% 
  ungroup() %>% 
  mutate(prop = n_female/total) %>% 
  arrange(-n_female) %>% 
  print(n = Inf)

# Correlation between general bills presented by female legislators vs. all bills presented
foo <- 
  bd_rep_sus %>% 
  group_by(state) %>% 
  summarise(n_female = sum(gender_gral == "Female"),
            total = n()) %>% 
  ungroup()  

cor.test(foo$n_female, foo$total)
  

# General bills presented by female legislators vs. all bills presented - Graph not included in the text
bd_rep_sus %>% 
  group_by(state) %>% 
  summarise(n_female = sum(gender_gral == "Female"),
            total = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = total, 
             y = n_female)) +
  geom_point() +
  geom_text_repel(aes(label = state))

# Share of bills presented by female legislators with respect to the total number of bills presented + Share of feminist bills presented by female legislators with respect to the total number of feminist bills presented by all legislators

# Please note that the analysis of this section uses the tibble bd_rep_sus_agg_fem

# Data

g_data <- 
  bd_rep_sus_agg_fem %>% 
  mutate(shr_bills_of_total = bills/bills_tot,
         shr_bills_of_total = ifelse(is.na(shr_bills_of_total), 0, shr_bills_of_total),
         shr_fem_bills_of_total = bills_fem/bills_fem_tot,
         shr_fem_bills_of_total = ifelse(is.na(shr_fem_bills_of_total), 0, shr_fem_bills_of_total)) %>%
  select(state, leg_period, shr_bills_of_total, shr_fem_bills_of_total) %>% 
  print(n = Inf)


# Mean and median of shr_bills_of_total
g_data %>% 
  group_by(leg_period) %>% 
  summarise(media = mean(shr_bills_of_total),
            mediana = median(shr_bills_of_total)) %>% 
  ungroup()

# Change in shr_bills_of_total
g_data %>% 
  group_by(state) %>% 
  mutate(change_shr_bills_of_total = shr_bills_of_total - lag(shr_bills_of_total)) %>% 
  ungroup() %>% 
  filter(!is.na(change_shr_bills_of_total)) %>% 
  arrange(-change_shr_bills_of_total) %>% 
  print(n = Inf)

# Mean and median of shr_fem_bills_of_total
g_data %>% 
  group_by(leg_period) %>% 
  summarise(media = mean(shr_fem_bills_of_total),
            mediana = median(shr_fem_bills_of_total)) %>% 
  ungroup()

# Change in shr_fem_bills_of_total
g_data %>% 
  group_by(state) %>% 
  mutate(change_shr_fem_bills_of_total = shr_fem_bills_of_total - lag(shr_fem_bills_of_total)) %>% 
  ungroup() %>% 
  filter(!is.na(change_shr_fem_bills_of_total)) %>% 
  arrange(-change_shr_fem_bills_of_total) %>% 
  print(n = Inf)


# Graph

g_1 <- 
  g_data %>% 
  pivot_longer(-c(state, leg_period)) %>% 
  mutate(leg_period = ifelse(leg_period == 1, "First", "Second"), 
         name = ifelse(name == "shr_bills_of_total", "Share of bills presented by female legislators with respect to the total number of bills presented by all legislators", "Share of feminist bills presented by female legislators with respect to the total number of feminist bills presented by all legislators")) %>% 
  arrange(state, name) %>% 
  group_by(state, name) %>% 
  mutate(line_start = ifelse(value < lag(value), 0, NA),
         line_end = ifelse( value < lag(value), 1, NA)) %>% 
  ungroup() %>% 
  ggplot(aes(x = value,
             y = fct_rev(state),
             color = as.factor(leg_period),
             shape = as.factor(leg_period))) +
  geom_segment(aes(x = line_start, y = fct_rev(state), xend = line_end, yend = fct_rev(state)), colour = "grey90", size = 6, alpha = 0.4)  +
  geom_point(color = "grey30", size = 3) +
  facet_wrap(~ str_wrap(name, width = 62)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_shape_manual(values=c(1, 19)) +
  labs(x = NULL,
       y = NULL,
       color = "Legislative period",
       shape = "Legislative period") +
  theme_fdtsr +
  theme(panel.spacing = unit(1, "lines"),
        panel.border = element_rect(color = "grey70", fill = "transparent"),
        strip.text = element_text(size = 22))

g_2 <- 
  g_data %>% 
  pivot_longer(-c(state, leg_period)) %>% 
  mutate(leg_period = ifelse(leg_period == 1, "First\nLegislative\nPeriod", "Second\nLegislative\nPeriod")) %>% 
  filter(name == "shr_bills_of_total") %>% 
  ggplot(aes(x = value, 
             y = fct_rev(leg_period),
             fill = leg_period)) +
  geom_boxplot() +
  scale_fill_manual(values = c("white", "grey30")) +
  scale_x_continuous(breaks = seq(0, 1, 0.1),
                     limits = c(-0.05, 1.05), 
                     expand = c(0, 0)) +
  labs(x = "\n",
       y = NULL,
       fill = "Legislative period") +
  theme_fdtsr +
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


g_3 <- 
  g_data %>%   
  pivot_longer(-c(state, leg_period)) %>% 
  mutate(leg_period = ifelse(leg_period == 1, "First\nLegislative\nPeriod", "Second\nLegislative\nPeriod")) %>% 
  filter(name == "shr_fem_bills_of_total") %>% 
  ggplot(aes(x = value, 
             y = fct_rev(leg_period),
             fill = leg_period)) +
  geom_boxplot() +
  scale_fill_manual(values = c("white", "grey30")) +
  scale_x_continuous(breaks = seq(0, 1, 0.1),
                     limits = c(-0.05, 1.05), 
                     expand = c(0, 0)) +
  labs(x = "\n",
       y = NULL,
       fill = "Legislative period") +
  theme_fdtsr +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


plot_grid(g_1, 
          plot_grid(NULL, g_2, NULL, g_3, ncol = 4, rel_widths = c(0.05, 0.428, 0.008, 0.377)), 
          ncol = 1,
          rel_heights = c(0.7, 0.3)) +
  theme(plot.background = element_rect(fill = "white", color = "white"))


ggsave("03_vis/ch_5/figure_5.1.png", dpi = 300, width = 16, height = 14)
ggsave("03_vis/ch_5/figure_5.1.tiff", dpi = 300, width = 16, height = 14)

  
# Analysis of the topics covered by the feminists bills presented by female legislators
bd_rep_sus %>% 
  filter(gender_gral == "Female" & feminist == "Yes") %>% 
  group_by(leg_period) %>% 
  count(agenda) %>% 
  ungroup() %>% 
  group_by(agenda) %>% 
  mutate(change = n - lag(n),
         per_change = (n - lag(n))/lag(n)*100,
         total =sum(n)) %>% 
  ungroup() %>% 
  mutate(per = n/total*100) %>% 
  arrange(agenda, leg_period)


g_1 <- 
  bd_rep_sus %>% 
  filter(gender_gral == "Female" & feminist == "Yes") %>% 
  count(agenda) %>% 
  mutate(per = n/sum(n)*100,
         etiqueta_gde = ifelse(n > 200, 
                               str_c(comma(n, accuracy = 2),
                                     " (", round(per, 1), "%)"),
                               ""),
         etiqueta_peque = ifelse(n < 200, 
                                 str_c(comma(n, accuracy = 2),
                                       " (", round(per, 1), "%)"),
                                 "")) %>% 
  ggplot(aes(x = n,
             y = fct_reorder(agenda, n))) +
  geom_col(fill = "grey30") +
  geom_text(aes(label = etiqueta_gde),
            size = 5, hjust = 1.1, color = "white") +
  geom_text(aes(label = etiqueta_peque),
            size = 5, hjust = -0.1, color = "grey30") +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "",
       y = "") +
  theme_fdtsr +
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank())

g_2 <- 
  bd_rep_sus %>% 
  filter(gender_gral == "Female" & feminist == "Yes") %>% 
  group_by(leg_period) %>% 
  count(agenda) %>% 
  ungroup() %>% 
  group_by(agenda) %>% 
  mutate(total =sum(n)) %>% 
  ungroup() %>% 
  mutate(per = n/total*100,
         leg_period = ifelse(leg_period == 1, "First Legislative Period", "Second Legislative Period"),
         legend = ifelse(leg_period == "Second Legislative Period", str_wrap(agenda, width = 15), NA)) %>% 
  ggplot(aes(x = leg_period,
             y = n, 
             group = agenda)) +
  geom_line(color = "grey30") +
  geom_point(color = "grey30", size = 3) +
  geom_text_repel(aes(label = legend), color = "grey40", size = 4, fontface = "bold", lineheight = 0.9, segment.curvature = -0.1, segment.ncp = 3, segment.angle = 20, nudge_x = .15, box.padding = 0.5, nudge_y = 1) +
  scale_y_continuous(breaks = seq(0, 700, 100)) +
  labs(x = "",
       y = "Number of bills\n") +
  theme_fdtsr

plot_grid(g_1, NULL, g_2, nrow = 1, rel_widths = c(0.45, 0.02, 0.45)) +
  theme(plot.background = element_rect(fill = "white", color = "white"))

ggsave("03_vis/ch_5/figure_5.2.png", dpi = 300, width = 16, height = 9)
ggsave("03_vis/ch_5/figure_5.2.tiff", dpi = 300, width = 16, height = 9)

### Bills presented by female legislators that where approved ----

# General bills presented by female legislators that where approved
bd_rep_sus %>% 
  summarise(n = sum(gender_gral == "Female" & status == "Approved"))

# Feminist bills presented by female legislators that where approved
bd_rep_sus %>% 
  summarise(n = sum(gender_gral == "Female" & feminist == "Yes" & status == "Approved"))


# Share of bills presented by female legislators that were approved with respect to the total number of bills presented by female legislators + Share of feminist bills presented by female legislators that were approved with respect to the total number of feminist bills presented by female legislators 

# Please note that theanalysis in this section uses the tibble bd_rep_sus_agg_fem

# Data

g_data <- 
  bd_rep_sus_agg_fem %>% 
  mutate(shr_bills_pass = bills_pass/bills, 
         shr_bills_pass = ifelse(is.na(shr_bills_pass), 0, shr_bills_pass),
         shr_fem_bills_pass = bills_fem_pass/bills_fem,
         shr_fem_bills_pass = ifelse(is.na(shr_fem_bills_pass), 0, shr_fem_bills_pass)) %>% 
  select(state, leg_period, shr_bills_pass, shr_fem_bills_pass) %>% 
  print(n = Inf)


# Mean and median of shr_bills_pass
g_data %>% 
  group_by(leg_period) %>% 
  summarise(media = mean(shr_bills_pass),
            mediana = median(shr_bills_pass)) %>% 
  ungroup()

# Change in shr_bills_pass
g_data %>% 
  group_by(state) %>% 
  mutate(change_shr_bills_pass = shr_bills_pass - lag(shr_bills_pass)) %>% 
  ungroup() %>% 
  filter(!is.na(change_shr_bills_pass)) %>% 
  arrange(-change_shr_bills_pass) %>% 
  print(n = Inf)

# Mean and median of shr_fem_bills_pass
g_data %>% 
  group_by(leg_period) %>% 
  summarise(media = mean(shr_fem_bills_pass),
            mediana = median(shr_fem_bills_pass)) %>% 
  ungroup()

# Change in shr_fem_bills_pass
g_data %>% 
  group_by(state) %>% 
  mutate(change_shr_fem_bills_pass = shr_fem_bills_pass - lag(shr_fem_bills_pass)) %>% 
  ungroup() %>% 
  filter(!is.na(change_shr_fem_bills_pass)) %>% 
  arrange(-change_shr_fem_bills_pass) %>% 
  print(n = Inf)



# Graph

g_1 <- 
  g_data %>% 
  pivot_longer(-c(state, leg_period)) %>% 
  mutate(leg_period = ifelse(leg_period == 1, "First", "Second"), 
         name = ifelse(name == "shr_bills_pass", "Share of bills presented by female legislators that were approved with respect to the total number of bills presented by female legislators", "Share of feminist bills presented by female legislators that were approved with respect to the total number of feminist bills presented by female legislators")) %>% 
  group_by(state, name) %>% 
  mutate(line_start = ifelse(value < lag(value), 0, NA),
         line_end = ifelse( value < lag(value), 1, NA)) %>% 
  ungroup() %>% 
  ggplot(aes(x = value,
             y = fct_rev(state),
             color = as.factor(leg_period),
             shape = as.factor(leg_period))) +
  geom_segment(aes(x = line_start, y = fct_rev(state), xend = line_end, yend = fct_rev(state)), colour = "grey90", size = 6, alpha = 0.4)  +
  geom_point(color = "grey30", size = 3) +
  facet_wrap(~ str_wrap(name, width = 62)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_shape_manual(values=c(1, 19)) +
  labs(x = NULL,
       y = NULL,
       color = "Legislative period",
       shape = "Legislative period") +
  theme_fdtsr +
  theme(panel.spacing = unit(1, "lines"),
        panel.border = element_rect(color = "grey70", fill = "transparent"),
        strip.text = element_text(size = 22))

g_2 <- 
  g_data %>% 
  pivot_longer(-c(state, leg_period)) %>% 
  mutate(leg_period = ifelse(leg_period == 1, "First\nLegislative\nPeriod", "Second\nLegislative\nPeriod")) %>% 
  filter(name == "shr_bills_pass") %>% 
  ggplot(aes(x = value, 
             y = fct_rev(leg_period),
             fill = leg_period)) +
  geom_boxplot() +
  scale_fill_manual(values = c("white", "grey30")) +
  scale_x_continuous(breaks = seq(0, 1, 0.1),
                     limits = c(-0.05, 1.05), 
                     expand = c(0, 0)) +
  labs(x = "\n",
       y = NULL,
       fill = "Legislative period") +
  theme_fdtsr +
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


g_3 <- 
  g_data %>% 
  pivot_longer(-c(state, leg_period)) %>% 
  mutate(leg_period = ifelse(leg_period == 1, "First\nLegislative\nPeriod", "Second\nLegislative\nPeriod")) %>% 
  filter(name == "shr_fem_bills_pass") %>% 
  ggplot(aes(x = value, 
             y = fct_rev(leg_period),
             fill = leg_period)) +
  geom_boxplot() +
  scale_fill_manual(values = c("white", "grey30")) +
  scale_x_continuous(breaks = seq(0, 1, 0.1),
                     limits = c(-0.05, 1.05), 
                     expand = c(0, 0)) +
  labs(x = "\n",
       y = NULL,
       fill = "Legislative period") +
  theme_fdtsr +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


plot_grid(g_1, 
          plot_grid(NULL, g_2, NULL, g_3, ncol = 4, rel_widths = c(0.05, 0.428, 0.008, 0.377)), 
          ncol = 1,
          rel_heights = c(0.7, 0.3)) +
  theme(plot.background = element_rect(fill = "white", color = "white"))


ggsave("03_vis/ch_5/figure_5.3.png", dpi = 300, width = 16, height = 14)
ggsave("03_vis/ch_5/figure_5.3.tiff", dpi = 300, width = 16, height = 14)



# Share of bills presented by female legislators that were approved with respect to the total number of bills approved + Share of feminist bills presented by female legislators that were approved with respect to the total number of feminist bills approved 

# Please note that theanalysis in this section uses the tibble bd_rep_sus_agg_fem

# Data

g_data <- 
  bd_rep_sus_agg_fem %>% 
  mutate(shr_bills_pass = bills_pass/bills_pass_tot, 
         shr_bills_pass = ifelse(is.na(shr_bills_pass), 0, shr_bills_pass),
         shr_fem_bills_pass = bills_fem_pass/bills_fem_pass_tot,
         shr_fem_bills_pass = ifelse(is.na(shr_fem_bills_pass), 0, shr_fem_bills_pass)) %>% 
  select(state, leg_period, shr_bills_pass, shr_fem_bills_pass) %>% 
  print(n = Inf)


# Mean and median of shr_bills_pass
g_data %>% 
  group_by(leg_period) %>% 
  summarise(media = mean(shr_bills_pass),
            mediana = median(shr_bills_pass)) %>% 
  ungroup()

# Change in shr_bills_pass
g_data %>% 
  group_by(state) %>% 
  mutate(change_shr_bills_pass = shr_bills_pass - lag(shr_bills_pass)) %>% 
  ungroup() %>% 
  filter(!is.na(change_shr_bills_pass)) %>% 
  arrange(-change_shr_bills_pass) %>% 
  print(n = Inf)

# Mean and median of shr_fem_bills_pass
g_data %>% 
  group_by(leg_period) %>% 
  summarise(media = mean(shr_fem_bills_pass),
            mediana = median(shr_fem_bills_pass)) %>% 
  ungroup()

# Change in shr_fem_bills_pass
g_data %>% 
  group_by(state) %>% 
  mutate(change_shr_fem_bills_pass = shr_fem_bills_pass - lag(shr_fem_bills_pass)) %>% 
  ungroup() %>% 
  filter(!is.na(change_shr_fem_bills_pass)) %>% 
  arrange(-change_shr_fem_bills_pass) %>% 
  print(n = Inf)




# Graph

g_1 <- 
  g_data %>% 
  pivot_longer(-c(state, leg_period)) %>% 
  mutate(leg_period = ifelse(leg_period == 1, "First", "Second"), 
         name = ifelse(name == "shr_bills_pass", "Share of bills presented by female legislators that were approved with respect to the total number of bills approved", "Share of feminist bills presented by female legislators that were approved with respect to the total number of feminist bills approved")) %>% 
  group_by(state, name) %>% 
  mutate(line_start = ifelse(value < lag(value), 0, NA),
         line_end = ifelse( value < lag(value), 1, NA)) %>% 
  ungroup() %>% 
  ggplot(aes(x = value,
             y = fct_rev(state),
             color = as.factor(leg_period),
             shape = as.factor(leg_period))) +
  geom_segment(aes(x = line_start, y = fct_rev(state), xend = line_end, yend = fct_rev(state)), colour = "grey90", size = 6, alpha = 0.4)  +
  geom_point(color = "grey30", size = 3) +
  facet_wrap(~ str_wrap(name, width = 62)) +
  scale_x_continuous(breaks = seq(0, 1, 0.1)) +
  scale_shape_manual(values=c(1, 19)) +
  labs(x = NULL,
       y = NULL,
       color = "Legislative period",
       shape = "Legislative period") +
  theme_fdtsr +
  theme(panel.spacing = unit(1, "lines"),
        panel.border = element_rect(color = "grey70", fill = "transparent"),
        strip.text = element_text(size = 22))

g_2 <- 
  g_data %>% 
  pivot_longer(-c(state, leg_period)) %>% 
  mutate(leg_period = ifelse(leg_period == 1, "First\nLegislative\nPeriod", "Second\nLegislative\nPeriod")) %>% 
  filter(name == "shr_bills_pass") %>% 
  ggplot(aes(x = value, 
             y = fct_rev(leg_period),
             fill = leg_period)) +
  geom_boxplot() +
  scale_fill_manual(values = c("white", "grey30")) +
  scale_x_continuous(breaks = seq(0, 1, 0.1),
                     limits = c(-0.05, 1.05), 
                     expand = c(0, 0)) +
  labs(x = "\n",
       y = NULL,
       fill = "Legislative period") +
  theme_fdtsr +
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


g_3 <- 
  g_data %>% 
  pivot_longer(-c(state, leg_period)) %>% 
  mutate(leg_period = ifelse(leg_period == 1, "First\nLegislative\nPeriod", "Second\nLegislative\nPeriod")) %>% 
  filter(name == "shr_fem_bills_pass") %>% 
  ggplot(aes(x = value, 
             y = fct_rev(leg_period),
             fill = leg_period)) +
  geom_boxplot() +
  scale_fill_manual(values = c("white", "grey30")) +
  scale_x_continuous(breaks = seq(0, 1, 0.1),
                     limits = c(-0.05, 1.05), 
                     expand = c(0, 0)) +
  labs(x = "\n",
       y = NULL,
       fill = "Legislative period") +
  theme_fdtsr +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "cm"))


plot_grid(g_1, 
          plot_grid(NULL, g_2, NULL, g_3, ncol = 4, rel_widths = c(0.05, 0.428, 0.008, 0.377)), 
          ncol = 1,
          rel_heights = c(0.7, 0.3)) +
  theme(plot.background = element_rect(fill = "white", color = "white"))


ggsave("03_vis/ch_5/figure_5.4.png", dpi = 300, width = 16, height = 14)
ggsave("03_vis/ch_5/figure_5.4.tiff", dpi = 300, width = 16, height = 14)
