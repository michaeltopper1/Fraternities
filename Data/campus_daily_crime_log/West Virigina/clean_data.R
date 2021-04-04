
library(tidyverse)
library(pdftools)
library(lubridate)

load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/West Virigina/crime.rda")

west_virginia <- final_crime %>% 
  rename("incident" = description,
         "date_occurred" = incident_d_t,
         "date_reported" = reported_d_t) %>% 
  extract(date_reported, into = "time_reported", "(\\d{1,2}:\\d\\d)", remove = F) %>% 
  extract(date_occurred, into = "time_occurred", "(\\d{1,2}:\\d\\d)", remove = F) %>% 
  extract(date_reported, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d{2,4})") %>% 
  extract(date_occurred, "date_occurred", "(\\d{1,2}/\\d{1,2}/\\d{2,4})") %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(university = "West Virginia University", location = NA)

write_csv(west_virginia, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/westvirginia.csv")
