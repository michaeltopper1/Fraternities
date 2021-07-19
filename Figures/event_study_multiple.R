library(tidyverse)
library(lubridate)


weekly_crime <- read_csv("Created Data/xMaster_data_2021/weekly_panel.csv")

weekly_crime <- weekly_crime %>% 
  mutate(closure_1_floor = lubridate::floor_date(closure_1, unit = "week", week_start = 1)) %>%
  mutate(closure_2_floor = floor_date(closure_2, unit = "week",week_start = 1)) %>% 
  relocate(week, closure_1, closure_1_floor) %>% 
  mutate(beta_0 = ifelse(closure_1_floor == week | closure_2_floor == week, 1, 0)) %>% 
  mutate(beta_0 = ifelse(is.na(beta_0), 0, beta_0)) %>% 
  relocate(beta_0)

leads <- c(1:5)
lags <- c(1:5)

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
  mutate(beta_lead_binned = ifelse(beta_minus_5 ==1, 1, NA)) %>% 
  mutate(beta_lead_binned = ifelse(beta_minus_5 > 0, cumsum(beta_minus_5), NA)) %>% 
  relocate(beta_lead_binned, starts_with("beta")) %>% 
  fill(beta_lead_binned, .direction = "down")  %>% ungroup() %>% arrange(week)




weekly_crime <- weekly_crime %>% 
  group_by(university) %>% 
  arrange(university,week) %>% 
  mutate(beta_lag_binned = ifelse(beta_plus_5 == 1, 1, NA)) %>% 
  mutate(beta_lag_binned = ifelse(beta_plus_5 > 0, cumsum(beta_plus_5), NA)) %>% 
  relocate(beta_lag_binned) %>% 
  fill(beta_lag_binned, .direction = "down")  %>% ungroup()

weekly_crime <- weekly_crime %>% 
  mutate(across(c(beta_lead_binned, beta_lag_binned), ~ifelse(is.na(.), 0, .)))


