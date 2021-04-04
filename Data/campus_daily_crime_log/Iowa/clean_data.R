library(tidyverse)
library(readxl)
library(lubridate)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Iowa/Daily Crime Log 2013-2019.xls"

iowa <- read_xls(path, skip = 6) %>% 
  janitor::clean_names()

iowa <- iowa %>% 
  select(-starts_with('x'), -date_occurred_end, - case_status) %>% 
  extract(date_occurred, "time_occurred", "(\\d\\d:\\d\\d:\\d\\d)", remove = F) %>% 
  extract(date_reported, "time_reported", "(\\d\\d:\\d\\d:\\d\\d)", remove = F) %>% 
  extract(date_occurred, "date_occurred", "(\\d\\d\\d\\d-\\d\\d-\\d\\d)") %>% 
  extract(date_reported, "date_reported", "(\\d\\d\\d\\d-\\d\\d-\\d\\d)") %>% 
  mutate(across(starts_with("date"), ~ymd(.))) %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H:%M:%S"), format = "%H:%M"))) %>% 
  mutate(university = "University of Iowa") %>% 
  mutate(case_number = na.locf(case_number)) %>% 
  group_by(case_number, time_occurred, time_reported, university, date_reported, date_occurred, location) %>% 
  summarize(incident = paste(incident, collapse = " ")) %>% 
  ungroup()

write_csv(iowa, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/iowa.csv")
