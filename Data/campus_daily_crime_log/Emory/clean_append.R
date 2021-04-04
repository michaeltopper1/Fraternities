## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-03-25
##

library(tidyverse)
library(readxl)
library(lubridate)



path <- "Data/campus_daily_crime_log/Emory/2014 Daily Crime Log.xls"
emory_14 <- read_xls(path) %>% janitor::clean_names()

emory_14 <- emory_14 %>% 
  select(-status) %>% 
  rename("incident" = crime, "date_reported" = report_date) %>% 
  mutate(date_reported = mdy(date_reported)) %>% 
  mutate(date_occurred = NA, time_occurred = NA, time_reported = NA, university = "Emory University") %>% 
  mutate(case_number = as.double(case_number))


path_late <- "Data/campus_daily_crime_log/Emory/emory.csv"
emory_15_19 <- read_csv(path_late)

emory <- bind_rows(emory_14, emory_15_19)

write_csv(emory, file = "Data/campus_daily_crime_log/Cleaned_schools/emory.csv")

