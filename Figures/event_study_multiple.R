## Purpose of script: creates the 'multiple event' event study using weekly data
##
## Author: Michael Topper
##
## Date Last Edited: 2021-07-19
##

library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)

weekly_crime <- read_csv("Created Data/xMaster_data_2021/weekly_panel.csv")

weekly_crime <- weekly_crime %>% 
  mutate(closure_1_floor = lubridate::floor_date(closure_1, unit = "week", week_start = 1)) %>%
  mutate(closure_2_floor = floor_date(closure_2, unit = "week",week_start = 1)) %>% 
  relocate(week, closure_1, closure_1_floor) %>% 
  mutate(beta_0 = ifelse(closure_1_floor == week | closure_2_floor == week, 1, 0)) %>% 
  mutate(beta_0 = ifelse(is.na(beta_0), 0, beta_0)) %>% 
  relocate(beta_0)

lead_endpoint <- 8
lag_endpoint <- 8
leads <- c(1:lead_endpoint)
lags <- c(1:lag_endpoint)

for (i in leads) {
  column_name <- paste0("beta_minus_",i)
  weekly_crime <- weekly_crime %>% 
    mutate(!!sym(column_name) := ifelse(dplyr::lead(beta_0, n = i) == 1,1,0 ))
}

for (i in leads) {
  column_name <- paste0("beta_plus_",i)
  weekly_crime <- weekly_crime %>% 
    mutate(!!sym(column_name) := ifelse(dplyr::lag(beta_0, n = i) == 1,1,0 ))
}

weekly_crime <- weekly_crime %>% 
  mutate(across(starts_with("beta"), ~ifelse(is.na(.), 0, .)))

## now need to create the binned column which will be the sum of the 1s - create function for this. need to arrange this 

## this works
weekly_crime <- weekly_crime %>% 
  group_by(university) %>% 
  arrange(desc(week)) %>% 
  mutate(beta_lead_binned = ifelse(beta_minus_8 ==1, 1, NA)) %>% 
  mutate(beta_lead_binned = ifelse(beta_minus_8 > 0, cumsum(beta_minus_8), NA)) %>% 
  relocate(beta_lead_binned, starts_with("beta")) %>% 
  fill(beta_lead_binned, .direction = "down")  %>% ungroup() %>% arrange(week)




weekly_crime <- weekly_crime %>% 
  group_by(university) %>% 
  arrange(university,week) %>% 
  mutate(beta_lag_binned = ifelse(beta_plus_8 == 1, 1, NA)) %>% 
  mutate(beta_lag_binned = ifelse(beta_plus_8 > 0, cumsum(beta_plus_8), NA)) %>% 
  relocate(beta_lag_binned) %>% 
  fill(beta_lag_binned, .direction = "down")  %>% ungroup()

weekly_crime <- weekly_crime %>% 
  mutate(across(c(beta_lead_binned, beta_lag_binned), ~ifelse(is.na(.), 0, .)))

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv")

daily_crime <- daily_crime %>% 
  mutate(date_floor = floor_date(date, unit = "week", week_start = 1))

semester_numbers <- daily_crime %>% 
  select(semester_number, date_floor, university)

weekly_crime <- weekly_crime %>% 
  left_join(semester_numbers, by = c("week" = "date_floor", "university" = "university")) 



weekly_crime %>% 
  group_by(university, semester_number) %>% 
  mutate(uni_semester = cur_group_id()) %>% 
  ungroup() %>% 
  feols(alcohol_offense_per25 ~ beta_lead_binned + beta_minus_7 + beta_minus_6 + beta_minus_5 + beta_minus_4 + beta_minus_3 + beta_minus_2 + beta_0 +
          beta_plus_1 + beta_plus_2 + beta_plus_3 + beta_plus_4 + beta_plus_5 +
          beta_plus_6 + beta_plus_7 + beta_lag_binned| uni_semester, cluster = ~university, data = .) %>% 
  etable()

