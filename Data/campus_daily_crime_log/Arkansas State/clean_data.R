library(tidyverse)
library(lubridate)
library(readxl)


path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Arkansas State/A-State Police Daily Crime Log 2013-2019.csv"

arkansas <- read_csv(path) %>% 
  janitor::clean_names()

arkansas <- arkansas %>% 
  rename("date_reported" = report_date, "date_occurred" = earliest_date, "case_number" = incident,
         "incident" = offense_description) %>% 
  select(-report_time, - latest_date, - status) %>% 
  extract(date_reported, "time_reported", "(\\d{1,2}:\\d\\d)", remove = F) %>% 
  extract(date_reported, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d\\d)") %>% 
  extract(date_occurred, "time_occurred", "(\\d{1,2}:\\d\\d)", remove = F) %>% 
  extract(date_occurred, "date_occurred", "(\\d{1,2}/\\d{1,2}/\\d\\d)") %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H:%M"), format = "%H:%M"))) %>% 
  mutate(university = "Arkansas State University-Main Campus") %>% 
  mutate(across(starts_with("date"), ~mdy(.))) 

arkansas <- arkansas %>% 
  group_by(across(-incident)) %>% 
  summarize(incident = paste(incident, collapse = " ")) %>% 
  ungroup()

write_csv(arkansas, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/arkansas.csv")
