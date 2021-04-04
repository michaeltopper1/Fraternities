## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-03-09
##

library(tidyverse)
library(lubridate)
library(rvest)



directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/New Mexico"
setwd(directory)
files <- list.files(directory, pattern = ".csv$")

nm <- map(files, ~{read_csv(., col_names = F) %>% 
            janitor::clean_names() %>% 
            unlist() -> new_mex
          new_mex %>% str_trim() %>% str_detect("Details$") %>% which -> incidents
          new_mex %>% str_trim() %>% str_detect("Incident Date:") %>% which -> date
          new_mex %>% str_trim() %>% str_detect("CAD Number:") %>% which -> case_number
          new_mex %>% str_trim() %>% str_detect("Location:") %>% which -> location
          nm_incidents <- new_mex[incidents] %>% 
            as_tibble() %>% 
            rename("incident" = value)
          nm_date <- new_mex[date] %>% 
            as_tibble() %>% 
            extract(value, c("date_reported", "time_reported"), 
                    "(\\d{1,2}/\\d{1,2}/\\d{4})\\s(\\d{4})")
          nm_case <- new_mex[case_number] %>% 
            as_tibble() %>% 
            rename("case_number" = value)
          nm_location <- new_mex[location] %>% 
            as_tibble() %>% 
            rename("location" = value)
          nm <- bind_cols(nm_incidents, nm_date, nm_case)}) %>% 
  reduce(bind_rows)

nm <- nm %>% 
  mutate(date_occurred = NA, time_occurred = NA, university = "University of New Mexico-Main Campus",
         location = NA) %>% 
  separate(case_number, c("CAD", "case_number"), ":") %>% 
  select(-CAD) %>% 
  mutate(case_number = str_trim(case_number)) %>% 
  mutate(time_reported = format(strptime(time_reported, "%H%M"), format = "%H:%M")) %>% 
  mutate(date_reported = mdy(date_reported)) %>% 
  separate(incident, c("incident", "other"), "-") %>% 
  mutate(incident = str_trim(incident))  %>% 
  select(-other)

write_csv(nm, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/new_mexico.csv")
