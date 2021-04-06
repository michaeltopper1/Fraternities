library(tidyverse)
library(fixest) 
library(modelsummary)
library(ifc)
library(lubridate)


daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv",
                        guess_max = 50000)
weekly_crime <- read_csv("Created Data/xMaster_data_2021/weekly_panel.csv",
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

daily_crime %>% 
  feols(c( alcohol_offense,
          ihs_alcohol_offense, alcohol_offense_per100)~ treatment | university  + weekday + year + month, cluster = ~university,
        data = .) %>% 
  etable
daily_crime %>% 
  feols(c(alcohol_offense,
          ihs_alcohol_offense, alcohol_offense_per100)~ treatment |  uni_month  + weekday + year, cluster = ~university,
        data = .) %>% 
  etable

weekly_crime %>% 
  feols(c(alcohol_offense,
          ihs_alcohol_offense, alcohol_offense_per100)~ treatment |  uni_month + year, cluster = ~university,
        data = .) %>% 
  etable
weekly_crime %>% 
  fepois(c(sexual_assault, sexual_assault_per100, ihs_sexual_assault, alcohol_offense, alcohol_offense_per100, ihs_alcohol_offense
  )~ treatment + ftime_total_undergrad +
    graduation_rate_total_cohort_ + 
    total_undergrad_black + total_undergrad_asian + 
    total_undergrad_hispanic |university + uni_month , cluster = ~university,
  data = .) %>% 
  etable()

daily_crime %>% 
  fepois(c(alcohol_offense, alcohol_offense_per100, ihs_alcohol_offense) ~ treatment|
          uni_year + uni_month + uni_weekday, cluster = ~university, data = .) %>% 
  etable()

weekly_crime_full <- read_csv("Created Data/xMaster_data_2021/weekly_panel_full.csv",
                              guess_max = 50000)
weekly_crime_full <- ifc::event_study_week(weekly_crime_full, 6)

weekly_crime_full <- weekly_crime_full %>% 
  mutate(month = month(week), year = year(week)) %>% 
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
weekly_crime_es %>% 
  feols(alcohol_offense~treatment_minus_6 + treatment_minus_5 +treatment_minus_4 + treatment_minus_3 +
          treatment_minus_2  + treatment_minus_1 + treatment +
          treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 +
          treatment_plus_5 + treatment_plus_6| university + month + year, cluster = ~university,
        data = .  ) %>% 
  summary()

weekly_crime_es <- event_study_week(weekly_crime, 6)
weekly_crime_ %>% 
  select(starts_with("treatment_minus"), treatment, starts_with("treatment_plus"), university) %>% 
  filter(university == "West Virginia University") %>% View()

weekly_crime_full %>% 
  distinct(university) %>% 
  pull()
