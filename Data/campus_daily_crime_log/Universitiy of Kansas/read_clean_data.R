## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-25
##

library(tidyverse)
library(tabulizer)
library(lubridate)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Universitiy of Kansas/Binder.pdf"

kansas <- pdf_text(path)

kansas <- kansas %>% 
  str_split('\n') %>% 
  unlist()

kansas %>% str_detect('^U\\d{2}') %>% which -> crime_index

## warning message ok - just because location is a pain in the butt - i got it down to only 11 rows filling poorly
kansas <- kansas[crime_index] %>% 
  as_tibble() %>% 
  separate(value, c("case_number", "date_occurred", "delete", "date_reported", "incident"), 
           '\\s{1,}', extra = "merge") %>% 
  separate(incident, c("incident", "location"), "\\s{2,}|\\d\\d", extra= "merge") %>% 
  select(-delete) %>% 
  mutate(university = "University of Kansas") %>% 
  extract(date_occurred, "date_occurred", "(\\d{1,2}/\\d{1,2}/\\d{4})") %>% 
  extract(date_reported, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d{4})") %>% 
  mutate(across(starts_with("date"), ~ mdy(.))) %>% 
  mutate(time_occurred = NA, time_reported = NA)

write_csv(kansas, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/kansas.csv")
