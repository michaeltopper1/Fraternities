
library(tidyverse)
library(lubridate)
library(readxl)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/University of Central Florida/Crime Log 2013-2019.csv"

central_florida <- read_csv(path, skip = 9)[,1:10] %>% 
  janitor::clean_names()

central_florida <- central_florida %>% 
  select(-x2, -x3, -x6, -disposition, -x9) %>% 
  rename("case_number" = x1, "incident" = x4, 
         "date_reported" = x7, "date_occurred" = x8) %>% 
  extract(date_reported, "time_reported", "(\\d\\d:\\d\\d)", remove = F) %>% 
  extract(date_reported, "date_reported", "(\\d\\d/\\d{1,2}/\\d{2,4})") %>% 
  extract(date_occurred, "time_occurred", "(\\d\\d:\\d\\d)", remove = F) %>% 
  extract(date_occurred, "date_occurred", "(\\d\\d/\\d{1,2}/\\d{2,4})") %>% 
  mutate(university = "University of Central Florida") %>% 
  mutate(across(starts_with("date"), ~mdy(.)))

write_csv(central_florida, file = '/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/central_florida.csv')

