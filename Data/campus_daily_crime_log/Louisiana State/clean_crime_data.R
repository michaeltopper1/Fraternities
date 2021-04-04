
library(tidyverse)
library(lubridate)
library(glue)

## loading in all the data
load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Louisiana State/2013.rda")
load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Louisiana State/2014.rda")
load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Louisiana State/2015.rda")
load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Louisiana State/2016.rda")
load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Louisiana State/2017_2019.rda")

## binding together the data
louisiana_state <- bind_rows(
  crime_2013, crime_2014, crime_2015,
  crime_2016, crimes_2017_2019
)

## first cleaning the column names and getting only the columns I need
louisiana_state <- louisiana_state %>% 
  rename("date_reported" = date_report, "date_occurred" = date_incident, "incident" = offense) %>% 
  select(-date_incident_end) %>% 
  mutate(date_occurred = str_trim(date_occurred)) %>% 
  extract(date_occurred, into = c("time_occurred"), regex = "(\\d{1,2}:\\d\\d:\\d\\d[AP]M)", remove = F) %>% 
  extract(date_occurred, into = "date_occurred", regex = "(\\d{1,2}/\\d{1,2}/\\d\\d\\d\\d)") %>% 
  mutate(date_reported = mdy(date_reported)) %>% 
  mutate(date_occurred = mdy(date_occurred)) %>% 
  mutate(incident = gsub(pattern = "^.{1,}\\s-\\s", "", incident)) %>% ## this was to clean up the incidents
  mutate(incident = gsub(pattern = "^.{1,}-\\s", "", incident)) %>% 
  mutate(incident = gsub(pattern = "^.{1,}\\s-", "", incident)) %>% 
  mutate(incident = gsub(pattern = "^.{1,}-", "", incident))  
  

## most of this was spent cleaning out the time column which was a HUGE pain. 
louisiana_state <- louisiana_state %>% 
  extract(time_occurred, into = "am_pm", regex = "([AP]M)", remove = F) %>% 
  mutate(time_occurred = gsub("[AP]M", "", time_occurred)) %>%  
  mutate(time_occurred = format(strptime(time_occurred,"%l:%M"), format = "%H:%M")) %>% 
  mutate(time_occurred = glue("{time_occurred} {am_pm}")) %>% 
  mutate(time_occurred = gsub("^00", "12", time_occurred)) %>% 
  select(-am_pm) %>% 
  mutate(time_occurred = format(strptime(time_occurred,"%I:%M %p"), format = "%H:%M")) %>% 
  mutate(time_reported = NA, university = "Louisiana State University and Agricultural & Mechanical College") %>% 
  mutate(location = NA)
  

write_csv(louisiana_state, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/louisiana_state.csv")