library(tidyverse)
library(fixest) 
library(modelsummary)
library(ifc)
library(lubridate)


daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv",
                        guess_max = 50000)
weekly_crime <- read_csv("Created Data/xMaster_data_2021/weekly_panel.csv",
                         guess_max = 50000)
weekly_crime_full <- read_csv("Created Data/xMaster_data_2021/weekly_panel_full.csv",
                              guess_max = 50000)
## putting in IHS transform, university by month and university by year fixed effects, and per100 outcomes
daily_crime <- daily_crime %>% 
  mutate(across(c(sexual_assault, alcohol_offense,
                  theft, robbery_burglary, drug_offense), list(ihs = ihs_transform),
                                                 .names = "{.fn}_{.col}")) %>% 
  group_by(university, month) %>% 
  mutate(uni_month = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(across(c(sexual_assault, alcohol_offense,
                  theft, robbery_burglary, drug_offense), ~./total_students_all * 100000,
                .names = '{.col}_per100')) %>% 
  group_by(university, year) %>% 
  mutate(uni_year = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, weekday) %>% 
  mutate(uni_weekday = cur_group_id()) %>% 
  ungroup()


weekly_crime <- weekly_crime %>% 
  mutate(across(c(sexual_assault, alcohol_offense,
                  theft, robbery_burglary, drug_offense), list(ihs = ihs_transform),
                .names = "{.fn}_{.col}")) %>% 
  group_by(university, month) %>% 
  mutate(uni_month = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(across(c(sexual_assault, alcohol_offense,
                  theft, robbery_burglary, drug_offense), ~./total_students_all * 100000,
                .names = '{.col}_per100')) %>% 
  group_by(university, year) %>% 
  mutate(uni_year = cur_group_id()) %>% 
  ungroup() 


weekly_crime %>% 
  filter(year > 2013) %>% 
  feols(c(alcohol_offense_per100, ihs_alcohol_offense) ~ treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year, cluster = ~university, data = .) %>% 
  etable


daily_crime %>% 
  filter(year > 2013) %>% 
  group_by(university, year, month) %>% 
  mutate(uni_year_month = cur_group_id()) %>% 
  ungroup() %>% 
  feols(c(alcohol_offense_per100, ihs_alcohol_offense) ~ treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_year_month, cluster = ~university, data = .) %>% 
  etable

weekly_crime %>% 
  group_by(university, month) %>% 
  summarize(alcohol_offense_per100 = mean(alcohol_offense_per100, na.rm = T), .groups = "drop") %>% 
  ggplot(aes(month, alcohol_offense_per100)) +
  geom_line() + 
  facet_wrap(~university)


es <- ifc::event_study_week(weekly_crime_full, 6)
es <- es %>% 
  mutate(month = month(week)) %>% 
  mutate(year = year(week)) %>% 
  mutate(across(c(sexual_assault, alcohol_offense,
                  theft, robbery_burglary, drug_offense), list(ihs = ihs_transform),
                .names = "{.fn}_{.col}")) %>% 
  group_by(university, month) %>% 
  mutate(uni_month = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(across(c(sexual_assault, alcohol_offense,
                  theft, robbery_burglary, drug_offense), ~./total_students_all * 100000,
                .names = '{.col}_per100')) %>% 
  group_by(university, year) %>% 
  mutate(uni_year = cur_group_id()) %>% 
  ungroup() 
es_1 <- es %>% 
  filter(year > 2013) %>% 
  filter(university %in% uni_list) %>% 
  # filter(month != 6 & month != 7 & month!= 8) %>% 
  feols(sexual_assault_per100 ~ treatment_minus_6 + 
          treatment_minus_5 + treatment_minus_4 +
          treatment_minus_3 + treatment_minus_2 +
          treatment_minus_1 + treatment +
          treatment_plus_1 + treatment_plus_2 + 
          treatment_plus_3 + treatment_plus_4 +
          treatment_plus_5 + treatment_plus_6|
          uni_month + year, cluster = ~university, data = . )
es %>% filter(university == "California Polytechnic State University-San Luis Obispo") %>% 
  relocate(starts_with("treatment_minus"), treatment, starts_with("treatment_plus")) %>% View()
es_1 %>% 
  coefplot()
uni_list <- weekly_crime %>% 
  distinct(university) %>% 
  pull()
uni_list <- uni_list[1:15]
