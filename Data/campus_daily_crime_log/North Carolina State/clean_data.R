## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-08
##

library(tidyverse)
library(lubridate)
library(readxl)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/North Carolina State/NCSU_blotter1.xlsx"

nc_state <- read_xlsx(path)

nc_state <- nc_state %>% 
  janitor::clean_names() %>% 
  rename("case_number" = id,
         "date_occurred" = occured_date,
         "time_occurred" = occured_time,
         "occurred_am" = occured_am_pm,
         "date_reported" = notdte,
         "time_reported" = nottme,
         "reported_am" = am_pm) %>% 
  select(-disposition, - narrative, -report_number) 


nc_state <- nc_state %>% 
  mutate(time_reported = glue::glue("{time_reported} {reported_am}")) %>% 
  extract(date_occurred, "date_occurred", "(\\d{1,2}/\\d{1,2}/\\d{2,4})") %>% 
  extract(date_reported, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d{2,4})") %>% 
  extract(time_occurred, "time_occurred", "(\\d{1,2}:\\d{1,2})") %>% 
  mutate(time_occurred = glue::glue("{time_occurred} {occurred_am}")) %>% 
  mutate(time_occurred = ifelse(time_occurred == "NA NA", NA, time_occurred)) %>% 
  select(-occurred_am, -reported_am) %>% 
  mutate(across(starts_with("time"), ~format(strptime(. , format = "%I:%M %p"), format = "%H:%M"))) %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(university = "North Carolina State University at Raleigh")
  

write_csv(nc_state, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/nc_state.csv")













