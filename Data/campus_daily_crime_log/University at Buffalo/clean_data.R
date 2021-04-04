## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-23
##

library(tidyverse)
library(readxl)
library(lubridate)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/University at Buffalo/University at Buffalo Police Department daily crime logs from January 1 2013 - December 31 2019.csv"

buffalo <- read_csv(path) %>% 
  janitor::clean_names() %>% select(-x1, -disposition)

buffalo <- buffalo %>% 
  rename("incident" = nature_of_crime, 
         "date_occurred" = date_and_time_of_crime) %>% 
  extract(date_reported, c("date_reported", "time_reported"), 
          "(\\d{1,2}/\\d{1,2}/\\d{4})\\s(\\d{1,2}:\\d{1,2})") %>% 
  extract(date_occurred, c("date_occurred", "time_occurred"),
          "(\\d{1,2}[/-]\\d{1,2}[/-]\\d{4})\\s(\\d{1,2}:\\d{1,2})") %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H:%M"), format = "%H:%M"))) %>% 
  mutate(university = "University at Buffalo")

write_csv(buffalo, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/buffalo.csv")
