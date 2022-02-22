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

median_alc_freq <- daily_crime %>% 
  group_by(university) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  distinct(university, frequency_alc) %>% 
  arrange(desc(frequency_alc)) %>% 
  pull(frequency_alc) %>% 
  quantile(c(0.5))

median_sex_freq <- daily_crime %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  distinct(university, frequency_sex) %>% 
  pull(frequency_sex) %>% 
  quantile(c(0.5))



es_14 %>% 
  mutate(across(starts_with("beta_lead"), ~ifelse(. == 1 & treatment == 1, 0, .))) %>% 
  mutate(across(starts_with("beta_lag"), ~ifelse(. ==1 & treatment == 1, 0, .))) %>% 
  mutate(lead_6 = ifelse(beta_lead_1 == 1 | beta_lead_2 == 1 | beta_lead_3 == 1 | beta_lead_4 == 1 | beta_lead_5 ==1 | beta_lead_6 == 1, 1, 0),
         lag_6 = ifelse(beta_lag_1 ==1 | beta_lag_2 == 1 | beta_lag_3 == 1 | beta_lag_4 == 1 | beta_lag_5 ==1 | beta_lag_6 ==1,1, 0)) %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, lead_6, lag_6, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc > median_alc_freq & lead_6 ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))
es_14 %>% 
  mutate(across(starts_with("beta_lead"), ~ifelse(. == 1 & treatment == 1, 0, .))) %>% 
  mutate(across(starts_with("beta_lag"), ~ifelse(. ==1 & treatment == 1, 0, .))) %>% 
  mutate(lead_6 = ifelse(beta_lead_1 == 1 | beta_lead_2 == 1 | beta_lead_3 == 1 | beta_lead_4 == 1 | beta_lead_5 ==1 | beta_lead_6 == 1, 1, 0),
         lag_6 = ifelse(beta_lag_1 ==1 | beta_lag_2 == 1 | beta_lag_3 == 1 | beta_lag_4 == 1 | beta_lag_5 ==1 | beta_lag_6 ==1,1, 0)) %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, lead_6, lag_6, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc > median_alc_freq & treatment ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))
es_14 %>% 
  mutate(across(starts_with("beta_lead"), ~ifelse(. == 1 & treatment == 1, 0, .))) %>% 
  mutate(across(starts_with("beta_lag"), ~ifelse(. ==1 & treatment == 1, 0, .))) %>% 
  mutate(lead_6 = ifelse(beta_lead_1 == 1 | beta_lead_2 == 1 | beta_lead_3 == 1 | beta_lead_4 == 1 | beta_lead_5 ==1 | beta_lead_6 == 1, 1, 0),
         lag_6 = ifelse(beta_lag_1 ==1 | beta_lag_2 == 1 | beta_lag_3 == 1 | beta_lag_4 == 1 | beta_lag_5 ==1 | beta_lag_6 ==1,1, 0)) %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, lead_6, lag_6, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc > median_alc_freq & lag_6 ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))




es_14 %>% 
  mutate(across(starts_with("beta_lead"), ~ifelse(. == 1 & treatment == 1, 0, .))) %>% 
  mutate(across(starts_with("beta_lag"), ~ifelse(. ==1 & treatment == 1, 0, .))) %>% 
  mutate(lead_6 = ifelse(beta_lead_1 == 1 | beta_lead_2 == 1 | beta_lead_3 == 1 | beta_lead_4 == 1 | beta_lead_5 ==1 | beta_lead_6 == 1, 1, 0),
         lag_6 = ifelse(beta_lag_1 ==1 | beta_lag_2 == 1 | beta_lag_3 == 1 | beta_lag_4 == 1 | beta_lag_5 ==1 | beta_lag_6 ==1,1, 0)) %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, lead_6, lag_6, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc > median_alc_freq & treatment ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))
es_14 %>% 
  mutate(across(starts_with("beta_lead"), ~ifelse(. == 1 & treatment == 1, 0, .))) %>% 
  mutate(across(starts_with("beta_lag"), ~ifelse(. ==1 & treatment == 1, 0, .))) %>% 
  mutate(lead_6 = ifelse(beta_lead_1 == 1 | beta_lead_2 == 1 | beta_lead_3 == 1 | beta_lead_4 == 1 | beta_lead_5 ==1 | beta_lead_6 == 1, 1, 0),
         lag_6 = ifelse(beta_lag_1 ==1 | beta_lag_2 == 1 | beta_lag_3 == 1 | beta_lag_4 == 1 | beta_lag_5 ==1 | beta_lag_6 ==1,1, 0)) %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, lead_6, lag_6, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc > median_alc_freq & lag_6 ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))
es_14 %>% 
  mutate(across(starts_with("beta_lead"), ~ifelse(. == 1 & treatment == 1, 0, .))) %>% 
  mutate(across(starts_with("beta_lag"), ~ifelse(. ==1 & treatment == 1, 0, .))) %>% 
  mutate(lead_6 = ifelse(beta_lead_1 == 1 | beta_lead_2 == 1 | beta_lead_3 == 1 | beta_lead_4 == 1 | beta_lead_5 ==1 | beta_lead_6 == 1, 1, 0),
         lag_6 = ifelse(beta_lag_1 ==1 | beta_lag_2 == 1 | beta_lag_3 == 1 | beta_lag_4 == 1 | beta_lag_5 ==1 | beta_lag_6 ==1,1, 0)) %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, lead_6, lag_6, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc > median_alc_freq & treatment ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))



es_14 %>% 
  mutate(across(starts_with("beta_lead"), ~ifelse(. == 1 & treatment == 1, 0, .))) %>% 
  mutate(across(starts_with("beta_lag"), ~ifelse(. ==1 & treatment == 1, 0, .))) %>% 
  mutate(lead_6 = ifelse(beta_lead_1 == 1 | beta_lead_2 == 1 | beta_lead_3 == 1 | beta_lead_4 == 1 | beta_lead_5 ==1 | beta_lead_6 == 1, 1, 0),
         lag_6 = ifelse(beta_lag_1 ==1 | beta_lag_2 == 1 | beta_lag_3 == 1 | beta_lag_4 == 1 | beta_lag_5 ==1 | beta_lag_6 ==1,1, 0)) %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, lead_6, lag_6, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc < median_alc_freq & lag_6 ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))
es_14 %>% 
  mutate(across(starts_with("beta_lead"), ~ifelse(. == 1 & treatment == 1, 0, .))) %>% 
  mutate(across(starts_with("beta_lag"), ~ifelse(. ==1 & treatment == 1, 0, .))) %>% 
  mutate(lead_6 = ifelse(beta_lead_1 == 1 | beta_lead_2 == 1 | beta_lead_3 == 1 | beta_lead_4 == 1 | beta_lead_5 ==1 | beta_lead_6 == 1, 1, 0),
         lag_6 = ifelse(beta_lag_1 ==1 | beta_lag_2 == 1 | beta_lag_3 == 1 | beta_lag_4 == 1 | beta_lag_5 ==1 | beta_lag_6 ==1,1, 0)) %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, lead_6, lag_6, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc < median_alc_freq & treatment ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))
es_14 %>% 
  mutate(across(starts_with("beta_lead"), ~ifelse(. == 1 & treatment == 1, 0, .))) %>% 
  mutate(across(starts_with("beta_lag"), ~ifelse(. ==1 & treatment == 1, 0, .))) %>% 
  mutate(lead_6 = ifelse(beta_lead_1 == 1 | beta_lead_2 == 1 | beta_lead_3 == 1 | beta_lead_4 == 1 | beta_lead_5 ==1 | beta_lead_6 == 1, 1, 0),
         lag_6 = ifelse(beta_lag_1 ==1 | beta_lag_2 == 1 | beta_lag_3 == 1 | beta_lag_4 == 1 | beta_lag_5 ==1 | beta_lag_6 ==1,1, 0)) %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, lead_6, lag_6, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc < median_alc_freq & lag_6 ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))





es_14 %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, week_before, week_after, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc > median_alc_freq & week_before ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))
es_14 %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, week_before, week_after, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc > median_alc_freq & treatment ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))
es_14 %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, week_before, week_after, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc > median_alc_freq & week_after ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))


es_14 %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, week_before, week_after, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc < median_alc_freq & week_before ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))
es_14 %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, week_before, week_after, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc < median_alc_freq & treatment ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))
es_14 %>% 
  select(university, alcohol_offense_per25, sexual_assault_per25, treatment, date, week_before, week_after, day_of_week) %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  ungroup() %>% 
  filter(frequency_alc < median_alc_freq & week_after ==1) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T))
