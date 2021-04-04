
library(tidyverse)
library(lubridate)
library(readxl)


## i used the csv file that I completed edited to append the hanging strings in the incident column
## i can now delete any hanging strings
north_florida <- read_csv("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/University of North Florida/crime_2015-2019.csv")

north_florida <- north_florida %>% 
  filter(!is.na(date_reported)) %>% 
  select(-disposition) %>% 
  extract(date_reported, c("date_reported", "time_reported"),
          "(\\d{1,2}/\\d{1,2}/\\d\\d)\\s(\\d{1,2}:\\d{1,2})") %>% 
  extract(date_occurred, c("date_occurred", "time_occurred"),
          "(\\d{1,2}/\\d{1,2}/\\d\\d)\\s(\\d{1,2}:\\d{1,2})") %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H:%M"), format = "%H:%M"))) %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(university = "University of North Florida")


write_csv(north_florida, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/northflorida.csv")
