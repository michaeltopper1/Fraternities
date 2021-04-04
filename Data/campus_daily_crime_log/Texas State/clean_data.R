## Purpose of script: cleans texas state
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-09
##

library(tidyverse)




load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Texas State/texas_1.rda")
load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Texas State/texas_2.rda")

crime_table <- crime_table %>% 
  select(-blotterid, -incidentdesc, -partydesc,
         -fire, -dispdesc, -inciden) %>% 
  rename("date_occurred" = incidentdate,
         "date_reported" = reportdate,
         "incident" = violationdesc,
         "location" = areadesc,
         "case_number" = casenum) %>% 
  extract(date_occurred, c("date_occurred", "time_occurred"),
          "(\\d{1,2}/\\d{1,2}/\\d{4})\\s(\\d{1,2}:\\d{1,2})") %>% 
  mutate(time_occurred = NA) %>% 
  extract(date_reported, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d{4})") %>% 
  mutate(time_reported = NA)

crime_table_2019 <- crime_table_2019 %>% 
  rename("incident" = V1,
         "case_number" = V2, 
         "date_reported" = V3,
         "date_occurred" = V4,
         "location" = V5,
         "dispostion" = V6) %>% 
  select(-dispostion) %>% 
  extract(date_reported, c("date_reported", "time_reported"), 
          "(\\d{1,2}/\\d{1,2}/\\d{2})\\s(\\d{2,4})") %>% 
  extract(date_occurred, c("date_occurred", "time_occurred"),
          "(\\d{1,2}/\\d{1,2}/\\d{2})\\s(\\d{2,4})") %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) 

texas_state <- bind_rows(crime_table, crime_table_2019)

texas_state <- texas_state %>% 
  mutate(across(starts_with("date"), ~mdy(.)), university = "Texas State University")

write_csv(texas_state, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/texas_state.csv")
