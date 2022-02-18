## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-17
##

library(tidyverse)

daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
daily_crime_all <- read_csv("created_data/xmaster_data/daily_panel_allschools.csv") %>% 
  filter(university %in% ifc::moratorium_schools() | university %in% ifc::never_treated_no_death())
es_14 <- ifc::event_study_day(daily_crime, 6, 7)
es_46 <- ifc::event_study_day(daily_crime, 3, 46)

es_14 %>% 
  mutate(across(starts_with("beta_lead"), ~ifelse(. == 1 & treatment == 1, 0, .))) %>% 
  mutate(across(starts_with("beta_lag"), ~ifelse(. ==1 & treatment == 1, 0, .))) %>% 
  mutate(lead_6 = ifelse(beta_lead_1 == 1 | beta_lead_2 == 1 | beta_lead_3 == 1 | beta_lead_4 == 1 | beta_lead_5 ==1 | beta_lead_6 == 1, 1, 0),
         lag_6 = ifelse(beta_lag_1 ==1 | beta_lag_2 == 1 | beta_lag_3 == 1 | beta_lag_4 == 1 | beta_lag_5 ==1 | beta_lag_6 ==1,1, 0)) %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, lead_6, lag_6, day_of_week) %>% 
  mutate(over_median = ifelse(university %in% over_median_alc, 1, 0)) %>% 
  group_by(over_median) %>% 
  filter(lead_6 ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T)) %>% 
  mutate(time = "lead_6")

es_14 %>% 
  mutate(across(starts_with("beta_lead"), ~ifelse(. == 1 & treatment == 1, 0, .))) %>% 
  mutate(across(starts_with("beta_lag"), ~ifelse(. ==1 & treatment == 1, 0, .))) %>% 
  mutate(lead_6 = ifelse(beta_lead_1 == 1 | beta_lead_2 == 1 | beta_lead_3 == 1 | beta_lead_4 == 1 | beta_lead_5 ==1 | beta_lead_6 == 1, 1, 0),
         lag_6 = ifelse(beta_lag_1 ==1 | beta_lag_2 == 1 | beta_lag_3 == 1 | beta_lag_4 == 1 | beta_lag_5 ==1 | beta_lag_6 ==1,1, 0)) %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, lead_6, lag_6, day_of_week) %>% 
  mutate(over_median = ifelse(university %in% over_median_alc, 1, 0)) %>% 
  group_by(over_median) %>% 
  filter(lag_6 ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T)) %>% 
  mutate(time = "lag_6")

es_14 %>% 
  mutate(over_median = ifelse(university %in% over_median_alc, 1, 0)) %>%
  group_by(over_median) %>% 
  summarize(across(c()))
es_46 %>% 
  filter(beta_lag_1 == 1) %>% 
  summarize(av_alc_after = mean(alcohol_offense_per25, na.rm = T))


daily_crime %>% 
  filter(if_any(c(lead_2, lead_1, treatment, lag_1, lag_2), ~. == 1)) 


median_alc_avg <- daily_crime %>% 
  group_by(university) %>% 
  mutate(average_alc = mean(alcohol_offense_per25, na.rm = T)) %>% 
  relocate(university, average_alc) %>% 
  distinct(university, average_alc) %>% 
  arrange(desc(average_alc)) %>% 
  pull(average_alc) %>% 
  quantile(c(0.5))

over_median_alc <- daily_crime %>% 
  group_by(university) %>% 
  mutate(average_alc = mean(alcohol_offense_per25, na.rm = T)) %>% 
  relocate(university, average_alc) %>% 
  distinct(university, average_alc) %>% 
  arrange(desc(average_alc)) %>% 
  mutate(over_alc_median = ifelse(average_alc > median_alc_avg, 1, 0)) %>% 
  ungroup() %>% 
  filter(over_alc_median == 1) %>% 
  pull(university)

median_sex_avg <- daily_crime %>% 
  group_by(university) %>% 
  mutate(average_sex = mean(sexual_assault_per25, na.rm = T)) %>% 
  distinct(university, average_sex) %>% 
  pull(average_sex) %>% 
  quantile(c(0.5))


daily_crime %>% 
  group_by(university) %>% 
  mutate(average_alc = mean(alcohol_offense_per25, na.rm = T)) %>% 
  mutate(over_median_alc = ifelse(average_alc > median_alc_avg, "High Volume", "Low Volume")) %>% 
  ungroup() %>% 
  mutate(treatment = ifelse(treatment == 1, "Moratorium", "No Moratorium")) %>% 
  group_by(treatment, over_median_alc, day_of_week) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T)) %>% 
  mutate(day_of_week = factor(day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>% 
  ggplot(aes(day_of_week, avg_alc)) +
  geom_col(aes(fill = as.factor(treatment)), position = "dodge")  +
  facet_wrap(~over_median_alc) +
  labs(x = " ", y = "Average Alcohol Offenses Per 25000 Students", fill = " ") +
  theme_minimal()

daily_crime %>% 
  group_by(university) %>% 
  mutate(average_sex = mean(sexual_assault_per25, na.rm = T)) %>% 
  mutate(over_median_sex = ifelse(average_sex > median_sex_avg, "High Volume", "Low Volume")) %>% 
  ungroup() %>% 
  mutate(treatment = ifelse(treatment == 1, "Moratorium", "No Moratorium")) %>% 
  mutate(weekend = ifelse(day_of_week %in% c("Fri", "Sat", "Sun"), "Weekend", "Weekday")) %>% 
  group_by(treatment, over_median_sex, weekend) %>% 
  summarize(avg_sex = mean(sexual_assault_per25, na.rm = T)) %>% 
  # mutate(day_of_week = factor(day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>% 
  ggplot(aes(weekend, avg_sex)) +
  geom_col(aes(fill = as.factor(treatment)), position = "dodge")  +
  facet_wrap(~over_median_sex, scales = "free_y") +
  labs(x = " ", y = "Average Sexual Assault Offenses Per 25000 Students", fill = " ") +
  theme_minimal()

daily_crime %>% 
  group_by(university) %>% 
  mutate(average_alc = mean(alcohol_offense_per25, na.rm = T)) %>% 
  mutate(over_median_alc = ifelse(average_alc > median_alc_avg, "High Volume", "Low Volume")) %>%
  ungroup() %>% 
  mutate(treatment = ifelse(treatment == 1, "Moratorium", "No Moratorium")) %>% 
  mutate(weekend = ifelse(day_of_week %in% c("Fri", "Sat", "Sun"), "Weekend", "Weekday")) %>% 
  group_by(treatment, over_median_alc, weekend) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T)) %>% 
  # mutate(day_of_week = factor(day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>% 
  ggplot(aes(weekend, avg_alc)) +
  geom_col(aes(fill = as.factor(treatment)), position = "dodge")  +
  facet_wrap(~over_median_alc) +
  labs(x = " ", y = "Average Alcohol Offenses Per 25000 Students", fill = " ") +
  theme_minimal()

daily_crime %>% 
  group_by(university) %>% 
  mutate(average_alc = mean(alcohol_offense_per25, na.rm = T)) %>% 
  mutate(over_median_alc = ifelse(average_alc > median_alc_avg, "High Volume", "Low Volume")) %>% 
  filter(lead_1 ==1 | treatment == 1 | lag_1 == 1) %>% 
  ungroup() %>% 
  select(university, date, lead_1, lag_1, alcohol_offense_per25, sexual_assault_per25, over_median_alc, treatment) %>% 
  pivot_longer(c(lead_1, treatment,lag_1), values_to = "values", names_to = "time") %>% 
  group_by(time, over_median_alc, values) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T)) %>% 
  filter(values == 1) %>% 
  mutate(time = factor(time, levels = c("lead_1", "treatment", "lag_1"))) %>% 
  ggplot(aes(time, avg_alc)) +
  geom_col() +
  facet_wrap(~over_median_alc, scales = "free_y") +
  theme_minimal()


es_14 %>% 
  mutate(across(starts_with("beta_lead"), ~ifelse(. == 1 & treatment == 1, 0, .))) %>% 
  mutate(across(starts_with("beta_lag"), ~ifelse(. ==1 & treatment == 1, 0, .))) %>% 
  mutate(over_median = ifelse(university %in% over_median_alc, 1, 0)) %>% 
  select(-beta_lead_binned, -beta_lag_binned) %>% 
  filter(if_any(matches("beta_lag|beta_lead|treatment"), ~. == 1)) %>% 
  select(university, date, beta_lag_1, beta_lag_2, beta_lag_3, beta_lag_4, beta_lag_5, beta_lag_6,
         beta_lead_1, beta_lead_2, beta_lead_3, beta_lead_4, beta_lead_5, beta_lead_6, alcohol_offense_per25, sexual_assault_per25, over_median, treatment) %>%
  pivot_longer(c(treatment, beta_lag_1, beta_lag_2, beta_lag_3, beta_lag_4, beta_lag_5, beta_lag_6,
                 beta_lead_1, beta_lead_2,beta_lead_3, beta_lead_4, beta_lead_5, beta_lead_6), values_to = "values", names_to = "time") %>% 
  group_by(time, over_median, values) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T)) %>% 
  filter(values == 1) %>% 
  mutate(time = factor(time, levels = c("beta_lead_6",
                                        "beta_lead_5", "beta_lead_4", "beta_lead_3", "beta_lead_2",
                                        "beta_lead_1", "treatment", "beta_lag_1", "beta_lag_2", 
                                        "beta_lag_3", "beta_lag_4", "beta_lag_5", "beta_lag_6"))) %>% 
  ggplot(aes(time, avg_alc)) +
  geom_point() +
  facet_wrap(~over_median, scales = "free_y") +
  theme_minimal()

