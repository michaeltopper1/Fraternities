## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-03-31
##

library(tidyverse)
library(lubridate)
library(readxl)

path_14 <- "Data/campus_daily_crime_log/Ohio State/Ohio_State_2014.csv"
ohio_14 <- read_csv(path_14) %>% 
  janitor::clean_names() %>% 
  rename(case_number = report_number, 
         date_reported = date_reported_reported_on,
         time_reported = time_reported_reported_on,
         date_occurred = date_occurred_incident_from,
         time_occurred = time_occurred_incident_from,
         incident = incident_type) %>% 
  mutate(university = "Ohio State University-Main Campus" )

ohio_14 <- ohio_14 %>% 
  filter(!str_detect(date_reported, "N/A")) %>% 
  mutate(across(starts_with("date"), ~ mdy(.))) %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H:%M"), format = "%H:%M"))) 

clean_read_15_18 <- function(path){
  output <- read_csv(path) %>% 
    janitor::clean_names() %>% 
    rename(case_number = report_number, 
           date_reported = date_reported_reported_on,
           date_occurred = date_occurred_incident_from,
           incident = incident_type) %>% 
    filter(!str_detect(date_reported, "N/A")) %>% 
    mutate(across(starts_with("date"), ~ mdy(.))) %>% 
    mutate(time_occurred = NA, time_reported = NA, location = NA,
           university ="Ohio State University-Main Campus" )
}

ohio_15 <- clean_read_15_18("Data/campus_daily_crime_log/Ohio State/Ohio_State_2015.csv")
ohio_16 <- clean_read_15_18("Data/campus_daily_crime_log/Ohio State/Ohio_State_2016.csv")
ohio_17 <- clean_read_15_18("Data/campus_daily_crime_log/Ohio State/Ohio_State_2017.csv")
ohio_18 <- clean_read_15_18("Data/campus_daily_crime_log/Ohio State/Ohio_State_2018.csv")


## read in last half of 18 and 19
path_19 <- "Data/campus_daily_crime_log/Ohio State/2018_2019.xls"
ohio_19 <- read_xls(path_19)
ohio_19 <- ohio_19 %>% 
  select(c(2:6)) %>% janitor::clean_names() 

ohio_19 <- ohio_19 %>% 
  rename(date_reported = date_time_reported,
         date_occurred = date_time_occurred,
         incident = general_location) %>% 
  select(-offenses) %>% 
  extract(date_reported, c("date_reported", "time_reported"), 
          "(\\d{4}-\\d{1,2}-\\d{1,2})\\s(\\d{1,2}:\\d{1,2})") %>% 
  extract(date_occurred, c("date_occurred", "time_occurred"), 
          "(\\d{4}-\\d{1,2}-\\d{1,2})\\s(\\d{1,2}:\\d{1,2})") %>% 
  mutate(university = "Ohio State University-Main Campus", 
         location = NA)

ohio_state_list <- list(ohio_14,
                        ohio_15,
                        ohio_16,
                        ohio_17,
                        ohio_18,
                        ohio_19)

ohio_state <- ohio_state_list %>% 
  map(., ~mutate(., across(everything(), ~as.character(.)))) %>% 
  reduce(bind_rows)


write_csv(ohio_state, file = "Data/campus_daily_crime_log/Cleaned_schools/ohio_state.csv")




