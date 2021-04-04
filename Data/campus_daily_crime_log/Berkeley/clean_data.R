## Purpose of script: Merging the clery Act, crime logs and ipeds 
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-26
##

library(tidyverse)
library(tabulizer)
library(glue)
not_all_na <- function(x) any(!is.na(x))

directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Berkeley"
setwd(directory)
files <- list.files(directory, pattern = "\\d\\d\\d\\d.xlsx")
berk_14_17 <- map(files, ~read_xlsx(., skip = 1) %>% 
      janitor::clean_names() %>% 
      select(-starts_with("x")) %>% 
      mutate(incident = glue("{category}--{incident}")) %>% 
      rename("date_reported" = date_time,
             "date_occurred" = date_time_occurred) %>% 
      select(-category, - synopsis, - disposition) %>% 
      extract(date_reported, "date_reported", "(\\d{2,4}-\\d{1,2}-\\d{1,2})") %>% 
      extract(date_occurred, c("date_occurred", "time_occurred"),
              "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s(\\d{1,2}:\\d{1,2})") %>% 
      mutate(date_reported = ymd(date_reported), date_occurred = mdy(date_occurred)) %>% 
      mutate(time_reported = NA, university ="University of California-Berkeley")) %>% 
  reduce(bind_rows)


## getting 2018 and 2019
not_all_na <- function(x) any(!is.na(x))
path_2018 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Berkeley/2018 - Daily Crime Log.csv"
berk_2018 <- read_csv(path_2018) %>% 
  janitor::clean_names()
path_2019 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Berkeley/2019 - Daily Crime Log.csv"
berk_2019 <- read_csv(path_2019) %>% 
  janitor::clean_names() 

berk_18_19 <- list(berk_2018, berk_2019)
  
berk_18_19 <- map(berk_18_19, ~filter(., !is.na(x7)) %>% 
      filter(x7 != "Category") %>% 
      select(where(not_all_na)) %>% 
      select(daily_crime_log, x2, x7) %>% 
      rename("date_reported" = daily_crime_log, 
             "time_reported" = x2,
             "incident" = x7) %>% 
      mutate(date_reported = mdy(date_reported), university = "University of California-Berkeley",
             date_occurred = NA, time_occurred = NA, location = NA) %>% 
      group_by(date_reported, incident) %>% 
      mutate(case_number = cur_group_id()) %>% 
      ungroup() %>% 
      mutate(case_number = paste(case_number,"-00", year(date_reported), sep = "")) %>% 
      mutate(time_reported = as.character(time_reported))) %>% 
  reduce(bind_rows)


berkeley <- bind_rows(berk_14_17, berk_18_19)

write_csv(berkeley, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/berkeley.csv")

         