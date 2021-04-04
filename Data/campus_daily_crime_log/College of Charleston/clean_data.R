## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-03-03
##

library(tidyverse)


## 2013
directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/College of Charleston/fwclery2013"
setwd(directory)
files <- list.files(directory)

college_of_charleston_13 <- map(files, ~read_xlsx(.) %>% janitor::clean_names() %>% 
      select(date_reported, description, location, incident_date, starts_with("oca")) %>% 
      filter(!is.na(date_reported)) %>% 
      extract(date_reported, c("date_reported", "time_reported"),
              "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
      extract(incident_date, c("date_occurred", "time_occurred"), 
              "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
      mutate(across(starts_with("date"), ~mdy(.))) %>% 
      mutate(university = "College of Charleston" ) %>% 
      rename("case_number" = oca_number,
             "incident" = description) %>% 
      mutate(across(starts_with("time"), ~paste("000", ., sep = ""))) %>% 
      extract(time_occurred, "time_occurred", "(\\d{4}$)") %>% 
      extract(time_reported, "time_reported", "(\\d{4})$") %>% 
      mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) %>% 
      filter(!is.na(date_reported))) %>% 
  reduce(bind_rows)


## 2014
directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/College of Charleston/fwclerylog2014"
setwd(directory)

files <- list.files(directory)

college_of_charleston_14 <- map(files, ~read_xlsx(.) %>% janitor::clean_names() %>% 
                                  select(date_reported, description, location, incident_date, starts_with("oca")) %>% 
                                  filter(!is.na(date_reported)) %>% 
                                  extract(date_reported, c("date_reported", "time_reported"),
                                          "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
                                  extract(incident_date, c("date_occurred", "time_occurred"), 
                                          "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
                                  mutate(across(starts_with("date"), ~mdy(.))) %>% 
                                  mutate(university = "College of Charleston" ) %>% 
                                  rename("case_number" = oca_number,
                                         "incident" = description) %>% 
                                  mutate(across(starts_with("time"), ~paste("000", ., sep = ""))) %>% 
                                  extract(time_occurred, "time_occurred", "(\\d{4}$)") %>% 
                                  extract(time_reported, "time_reported", "(\\d{4})$") %>% 
                                  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) %>% 
                                  filter(!is.na(date_reported))) %>% 
  reduce(bind_rows)

## 2015
directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/College of Charleston/fwclerylog2015"
setwd(directory)

files <- list.files(directory)

college_of_charleston_15 <- map(files, ~read_xlsx(.) %>% janitor::clean_names() %>% 
                                  select(date_reported, description, location, incident_date, starts_with("oca")) %>% 
                                  filter(!is.na(date_reported)) %>% 
                                  extract(date_reported, c("date_reported", "time_reported"),
                                          "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
                                  extract(incident_date, c("date_occurred", "time_occurred"), 
                                          "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
                                  mutate(across(starts_with("date"), ~mdy(.))) %>% 
                                  mutate(university = "College of Charleston" ) %>% 
                                  rename("case_number" = oca_number,
                                         "incident" = description) %>% 
                                  mutate(across(starts_with("time"), ~paste("000", ., sep = ""))) %>% 
                                  extract(time_occurred, "time_occurred", "(\\d{4}$)") %>% 
                                  extract(time_reported, "time_reported", "(\\d{4})$") %>% 
                                  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) %>% 
                                  filter(!is.na(date_reported))) %>% 
  reduce(bind_rows)


## 2016
directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/College of Charleston/fwclerylog2016"

setwd(directory)

files <- list.files(directory)

college_of_charleston_16 <- map(files, ~read_xlsx(.) %>% janitor::clean_names() %>% 
                                  select(date_reported, description, location, incident_date, starts_with("oca")) %>% 
                                  filter(!is.na(date_reported)) %>% 
                                  extract(date_reported, c("date_reported", "time_reported"),
                                          "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
                                  extract(incident_date, c("date_occurred", "time_occurred"), 
                                          "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
                                  mutate(across(starts_with("date"), ~mdy(.))) %>% 
                                  mutate(university = "College of Charleston" ) %>% 
                                  rename("case_number" = oca_number,
                                         "incident" = description) %>% 
                                  mutate(across(starts_with("time"), ~paste("000", ., sep = ""))) %>% 
                                  extract(time_occurred, "time_occurred", "(\\d{4}$)") %>% 
                                  extract(time_reported, "time_reported", "(\\d{4})$") %>% 
                                  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) %>% 
                                  filter(!is.na(date_reported))) %>% 
  reduce(bind_rows)



## 2017
directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/College of Charleston/fwclerylog2017"

setwd(directory)

files <- list.files(directory)

college_of_charleston_17 <- map(files, ~read_xlsx(.) %>% janitor::clean_names() %>% 
                                  select(date_reported, description, location, incident_date, starts_with("oca")) %>% 
                                  filter(!is.na(date_reported)) %>% 
                                  extract(date_reported, c("date_reported", "time_reported"),
                                          "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
                                  extract(incident_date, c("date_occurred", "time_occurred"), 
                                          "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
                                  mutate(across(starts_with("date"), ~mdy(.))) %>% 
                                  mutate(university = "College of Charleston" ) %>% 
                                  rename("case_number" = oca_number,
                                         "incident" = description) %>% 
                                  mutate(across(starts_with("time"), ~paste("000", ., sep = ""))) %>% 
                                  extract(time_occurred, "time_occurred", "(\\d{4}$)") %>% 
                                  extract(time_reported, "time_reported", "(\\d{4})$") %>% 
                                  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) %>% 
                                  filter(!is.na(date_reported))) %>% 
  reduce(bind_rows)


## 2018
directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/College of Charleston/fwclerylog2018"
setwd(directory)

files <- list.files(directory)
college_of_charleston_18 <- map(files, ~read_xlsx(.) %>% janitor::clean_names() %>% 
                                  select(date_reported, description, location, incident_date, starts_with("oca")) %>% 
                                  filter(!is.na(date_reported)) %>% 
                                  extract(date_reported, c("date_reported", "time_reported"),
                                          "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
                                  extract(incident_date, c("date_occurred", "time_occurred"), 
                                          "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
                                  mutate(across(starts_with("date"), ~mdy(.))) %>% 
                                  mutate(university = "College of Charleston" ) %>% 
                                  rename("case_number" = oca_number,
                                         "incident" = description) %>% 
                                  mutate(across(starts_with("time"), ~paste("000", ., sep = ""))) %>% 
                                  extract(time_occurred, "time_occurred", "(\\d{4}$)") %>% 
                                  extract(time_reported, "time_reported", "(\\d{4})$") %>% 
                                  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) %>% 
                                  filter(!is.na(date_reported))) %>% 
  reduce(bind_rows)


## 2019
directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/College of Charleston/fwclerylog2019"

setwd(directory)

files <- list.files(directory)

college_of_charleston_19 <- map(files, ~read_xlsx(.) %>% janitor::clean_names() %>% 
                                  select(date_reported, description, location, incident_date, starts_with("oca")) %>% 
                                  filter(!is.na(date_reported)) %>% 
                                  extract(date_reported, c("date_reported", "time_reported"),
                                          "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
                                  extract(incident_date, c("date_occurred", "time_occurred"), 
                                          "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,4})") %>% 
                                  mutate(across(starts_with("date"), ~mdy(.))) %>% 
                                  mutate(university = "College of Charleston" ) %>% 
                                  rename("case_number" = oca_number,
                                         "incident" = description) %>% 
                                  mutate(across(starts_with("time"), ~paste("000", ., sep = ""))) %>% 
                                  extract(time_occurred, "time_occurred", "(\\d{4}$)") %>% 
                                  extract(time_reported, "time_reported", "(\\d{4})$") %>% 
                                  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) %>% 
                                  filter(!is.na(date_reported))) %>% 
  reduce(bind_rows)

college_of_charleston <- bind_rows(college_of_charleston_13,
                                   college_of_charleston_14,
                                   college_of_charleston_15,
                                   college_of_charleston_16,
                                   college_of_charleston_17,
                                   college_of_charleston_18,
                                   college_of_charleston_19)

## unique incidences
college_of_charleston <- college_of_charleston %>% 
  group_by(across(-incident)) %>% distinct()
  summarize(incident = paste(incident, collapse = " ")) %>% 
  ungroup()

write_csv(college_of_charleston, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/college_of_charlestson.csv")
