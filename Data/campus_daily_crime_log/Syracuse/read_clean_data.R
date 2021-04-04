## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-25
##

library(tidyverse)
library(tabulizer)
library(pdftools)
library(lubridate)
library(readxl)

## 2014
path_2014 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Syracuse/2014 DCL.xlsx"
syracuse_14 <- read_xlsx(path_2014) %>% 
  janitor::clean_names()
syracuse_14 <- syracuse_14 %>% 
  rename("location" = general_location,
         "incident" = nature_classification,
         "date_reported" = date_time_reported,
         "date_occurred" = date_time_occurred) %>% 
  select(-case_offense_statute, - related_offenses, - disposition, - disposition_date) %>% 
  extract(date_occurred, c("date_occurred", "time_occurred"),
          '(\\d{1,2}/\\d{1,2}/\\d{4})\\s(\\d{1,2}:\\d{1,2})') %>% 
  extract(date_reported, c("date_reported", "time_reported"), 
          '(\\d{1,2}/\\d{1,2}/\\d{4})\\s(\\d{1,2}:\\d{1,2})') %>% 
  mutate(university = "Syracuse University")


### 2015
path_2015 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Syracuse/2015 DCL.xlsx"
syracuse_15 <- read_xlsx(path_2015) %>% 
  janitor::clean_names()

syracuse_15 <- syracuse_15 %>% 
  rename("location" = general_location,
         "incident" = nature_classification,
         "date_reported" = date_time_reported,
         "date_occurred" = date_time_occurred) %>% 
  select(-case_offense_statute, - related_offenses, - disposition, - disposition_date) %>% 
  extract(date_occurred, c("date_occurred", "time_occurred"),
          '(\\d{1,2}/\\d{1,2}/\\d{4})\\s(\\d{1,2}:\\d{1,2})') %>% 
  extract(date_reported, c("date_reported", "time_reported"), 
          '(\\d{1,2}/\\d{1,2}/\\d{4})\\s(\\d{1,2}:\\d{1,2})') %>% 
  mutate(university = "Syracuse University")



## cleaning and extracting syracuse 2016
syr_16 <- pdf_text("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Syracuse/2016 DCL.pdf")

syr_16 %>% str_split('\n') %>% unlist() %>% str_trim() ->syr_16

syr_16 %>% 
  str_detect("^.{1,}\\d{1,2}/\\d{1,2}/\\d{1,2}.{1,}\\d{1,2}:\\d{1,2}:\\d{1,2}") %>% which -> incidents
syr_16 %>% 
  str_detect("^.{1,}\\d{1,2}/\\d{1,2}/\\d{1,2}.{1,}\\d{1,2}:\\d{1,2}:\\d{1,2}") %>% which %>% magrittr::add(1) -> case_number
syra_incident <- syr_16[incidents] %>% 
  as_tibble() %>% 
  separate(value, c("location", "other"), "\\s{2,}", extra = "merge") %>% 
  extract(other, c("incident", "date_occurred", "time_occurred", "date_reported", "time_reported"),
          "(.{1,})\\s{1,}(\\d{1,2}/\\d{1,2}/\\d{4}).{1,}(\\d{1,2}:\\d{1,2}:\\d{1,2}).{1,}(\\d{1,2}/\\d{1,2}/\\d{4}).{0,}(\\d{1,2}:\\d{1,2}:\\d{1,2})")  %>% 
  mutate(across(starts_with("date"), ~ifelse(str_detect(., "^0"), paste("1", . ,sep = ""), .))) 
syra_case_number <- syr_16[case_number] %>% 
  as_tibble() %>% 
  extract(value, "case_number", "(\\d{1,})")

syra_16 <- bind_cols(syra_incident, syra_case_number)

syracuse_16 <- syra_16 %>% 
  mutate(across(starts_with("time"), ~format(strptime(., "%H:%M:%S"), format = '%H:%M'))) %>% 
  mutate(university = "Syracuse University") %>% 
  mutate(across(starts_with("time"), ~as.character(.)))

## cleaning 2017 - split into two files because formatting changes
syra_17 <- pdf_text("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Syracuse/2017 DCL.pdf")

syra_17 %>% str_split('\n') %>% unlist() %>% str_trim()-> syra_17

syra_17  <- syra_17[-3]

syracuse_17_1 <- syra_17 %>% 
  as_tibble() %>% 
  extract(value, c("open", "date_disposition", "case_number", "incident", "date_reported", "time_reported", "date_occurred"),
          "(.{1,})\\s{1,}(\\d{1,2}/\\d{1,2}/\\d{4})\\s{0,}(\\d{2}-\\d{1,})\\s{1,}(.{1,})\\s{1,}(\\d{1,2}/\\d{1,2}/\\d{4})\\s{1,}(\\d{2}:\\d{2}:\\d{2})\\s{0,}(\\d{1,2}/\\d{1,2}/\\d{4})") %>% 
  select(-open, -date_disposition) %>% 
  mutate(time_occurred = NA, university = "Syracuse University", location = NA)

path_2017_2 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Syracuse/2017 DCL_2.xlsx"

syracuse_17_2 <- read_xlsx(path_2017_2, skip = 4) %>% 
  janitor::clean_names()
syracuse_17_2 <- syracuse_17_2 %>% 
  select(-disposition_1) %>% 
  rename('date_reported' = disposition_2,
         'incident' = nature_classification,
         "location" = general_location) %>% 
  filter(!is.na(incident)) %>% 
  filter(!is.na(date_reported)) %>% 
  select(-date_time_5, -date_time_6) %>% 
  mutate(time_reported = NA, time_occurred = NA, date_occurred = NA) %>% 
  mutate(university = "Syracuse University")


## cleaning 2018
syra_18 <- pdf_text("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Syracuse/2018 DCL.pdf")

syra_18 %>% str_split('\n') %>% unlist() %>% str_trim()-> syra_18

syra_18  <- syra_18[-3]

syracuse_18 <- syra_18 %>% 
  as_tibble() %>% 
  extract(value, c("open", "date_disposition", "case_number", "incident", "date_reported", "time_reported", "date_occurred"),
          "(.{1,})\\s{1,}(\\d{1,2}/\\d{1,2}/\\d{4})\\s{0,}(\\d{2}-\\d{1,})\\s{1,}(.{1,})\\s{1,}(\\d{1,2}/\\d{1,2}/\\d{4})\\s{1,}(\\d{2}:\\d{2}:\\d{2})\\s{0,}(\\d{1,2}/\\d{1,2}/\\d{4})") %>% 
  select(-open, -date_disposition) %>% 
  mutate(time_occurred = NA, university = "Syracuse University", location = NA) 


### 2019

path_2019 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Syracuse/DCL 2019.xlsx"

syracuse_19 <- read_xlsx(path_2019) %>% 
  janitor::clean_names()

syracuse_19 <- syracuse_19 %>% 
  rename("location" = general_location,
         "incident" = nature_classification,
         "date_reported" = date_time_reported,
         "date_occurred" = date_time_occurred) %>% 
  select(-case_offense_statute, - related_offenses, - disposition, - disposition_date) %>% 
  extract(date_occurred, c("date_occurred", "time_occurred"),
          '(\\d{1,2}/\\d{1,2}/\\d{4})\\s(\\d{1,2}:\\d{1,2})') %>% 
  extract(date_reported, c("date_reported", "time_reported"), 
          '(\\d{1,2}/\\d{1,2}/\\d{4})\\s(\\d{1,2}:\\d{1,2})') %>% 
  mutate(university = "Syracuse University")


syracuse <- bind_rows(syracuse_14, syracuse_15, syracuse_16,
                      syracuse_17_1, syracuse_17_2, syracuse_18,
                      syracuse_19)

syracuse <- syracuse %>% 
  mutate(across(starts_with("date"), ~mdy(.)))

syracuse <- syracuse %>% 
  group_by(across(-incident)) %>% 
  summarize(incident = paste(incident, collapse = " "))

write_csv(syracuse, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/syracuse.csv")

