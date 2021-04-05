## Purpose of script: Clean Ohio University's records.
## Note: warnings due to weird excel file dots - nothing to worry about.
## Note: checked the total records and they match. 
## Author: Michael Topper
##
## Date Last Edited: 2021-04-05
##

library(tidyverse)
library(lubridate)

path <- "Data/campus_daily_crime_log/Ohio University/OUPD Logs 2014-2019"
files <- paste0(path,"/", list.files(path))

ohio <- map(files, ~read_csv(.) %>% janitor::clean_names() %>% 
      select(-case_number, -disposition) %>% 
      rename(location = address, case_number = cfs_number) %>% 
      extract(date_reported, c("date_reported", "time_reported"), "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,2}:\\d{1,2})") %>% 
      extract(date_occurred, c("date_occurred", "time_occurred"), "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,2}:\\d{1,2})") %>% 
      mutate(university = "Ohio University") %>% 
      mutate(across(starts_with("date"), ~mdy(.)))) %>% reduce(bind_rows)

ohio <- ohio %>% 
  mutate(incident = str_trim(gsub("CONVERSION_|CONVERSION|CONVRESION_|CONVERSOIN ", "", incident)))

write_csv(ohio, file = "Data/campus_daily_crime_log/Cleaned_schools/ohio_university.csv")
