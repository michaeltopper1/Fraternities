## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-23
##

library(tidyverse)
library(readxl)
library(lubridate)

directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Unversity of Vermont"

setwd(directory)
files <-  list.files(directory, pattern = "\\d\\d\\d\\d.csv$")

vermont_2013_2017 <- map(files, ~read_csv(., col_types = colnames(as.character(.))) %>% janitor::clean_names() %>% 
      rename("incident" = classification, "location" = general_location) %>% 
      select(-disposition) %>% 
      tidyr::extract(date_reported, into = "date_reported", regex = "(\\d{1,2}/\\d{1,2}/\\d{2})") %>% 
      tidyr::extract(date_occurred, into = "date_occurred", "(\\d{1,2}/\\d{1,2}/\\d{2})") %>% 
      mutate(across(starts_with("date"), ~mdy(.))) %>% 
      mutate(across(starts_with("time"), ~paste("0000", time_reported, sep = ""))) %>% 
      extract(time_reported, "time_reported", "(\\d{4}$)") %>% 
      extract(time_occurred, "time_occurred", "(\\d{4}$)") %>% 
      mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M")))) %>% 
  reduce(bind_rows)

path_2018 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Unversity of Vermont/2018CrimeLog.csv"

vermont_2018 <- path_2018 %>% 
  read_csv() %>% 
  janitor::clean_names() %>% 
  rename("incident" = classification, "location" = general_location) %>% 
  select(-disposition) %>% 
  extract(date_reported, into = "date_reported", regex = "(\\d{1,2}/\\d{1,2}/\\d{1,4})") %>% 
  extract(date_occurred, into = "date_occurred", "(\\d{1,2}/\\d{1,2}/\\d{1,4})") %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(across(starts_with("time"), ~paste("0000", time_reported, sep = ""))) %>% 
  extract(time_reported, "time_reported", "(\\d{4}$)") %>% 
  extract(time_occurred, "time_occurred", "(\\d{4}$)") %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M")))

path_2019 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Unversity of Vermont/2019CrimeLog.xlsx"

vermont_2019 <- path_2019 %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path_2019, skip = 1) %>% 
  reduce(bind_rows) %>% 
  janitor::clean_names() %>% 
  rename("incident" = classification, "location" = general_location) %>% 
  select(-disposition) %>% 
  tidyr::extract(date_reported, into = "date_reported", regex = "(\\d{1,2}/\\d{1,2}/\\d{2,4})") %>% 
  tidyr::extract(date_occurred, into = "date_occurred", "(\\d{1,2}/\\d{1,2}/\\d{2,4})") %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(across(starts_with("time"), ~paste("0000", time_reported, sep = ""))) %>% 
  extract(time_reported, "time_reported", "(\\d{4}$)") %>% 
  extract(time_occurred, "time_occurred", "(\\d{4}$)") %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M")))

vermont <- bind_rows(vermont_2013_2017, vermont_2018, vermont_2019)
vermont <- vermont %>% 
  mutate(university = "University of Vermont") %>% 
  rename("case_number" = incident_number)

vermont %>% 
  mutate(year = year(date_reported)) %>% 
  filter(year == 2018) %>% View()
write_csv(vermont, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/vermont.csv")
