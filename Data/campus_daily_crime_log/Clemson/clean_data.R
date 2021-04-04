## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-08
##

library(tidyverse)
library(pdftools)

path_14_18 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Clemson/2014-2018 Daily Crime Logs.pdf"

clemson_14 <- pdf_text(path_14_18)

clemson_14 %>% 
  str_split('\n') %>% 
  unlist() %>% str_trim() %>% str_to_lower() -> clemson_14

clemson_14 %>% 
  str_detect("^case status") %>% which ->case_status_incidices
clemson_14 <- clemson_14[-case_status_incidices]

clemson_14 %>% 
  str_detect("^page") %>% which ->page_indices

clemson_14 <- clemson_14[-page_indices]


clemson_14 %>% 
  str_detect("^p-") %>% which  -> case_number_indices
clemson_14 %>% 
  str_detect("^p-") %>% which %>% magrittr::add(1) -> incident_indices

clemson_14[case_number_indices] %>% 
  str_split_fixed('\\s{1,}', n = 6) %>% 
  as_tibble() %>% 
  rename("case_number" = V1,
         "date_reported" = V2,
         "time_reported" = V3,
         "date_occurred"= V4,
         "time_occurred" = V5,
         "location" = V6) %>% 
  separate(location, "location", "\\s{4,}") -> clemson_noincident
clemson_14[incident_indices] %>% 
  as_tibble() %>% 
  rename("incident" = value) -> incidents

clemson_14_18 <- bind_cols(clemson_noincident, incidents)

clemson_14_18 <- clemson_14_18 %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%I:%M:%S%p"), format = "%H:%M"))) %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>%
  separate(incident, c("crap", "incident"), sep = "\\s{1,}", extra = "merge") %>% 
  select(-crap) %>% 
  mutate(university = "Clemson University")



## now doing 2018-2019

path_2019 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Clemson/2018-2020 Daily Crime Log.xlsx"
clemson <- read_xlsx(path_2019, skip = 2)

clemson <- clemson %>% 
  janitor::clean_names() %>% 
  filter(case_number != "Case Number") %>% 
  filter(str_detect(case_number, "^P-")) %>% 
  select(-x6) 

clemson <- clemson %>% 
  rename("incident" = offense, 
         "date_reported" = reported_date_time,
         "date_occurred" = incident_start_date_time,
         "location" = incident_location) %>% 
  select(-disposition)

clemson <- clemson %>% 
  extract(date_reported, into = c("date_reported", "time_reported"),
         regex=  "(\\d{1,}/\\d{1,}/\\d{2})\\s{1,}(\\d\\d:\\d\\d)") %>% 
  extract(date_occurred, into = c("date_occurred", "time_occurred"),
          regex = "(\\d{1,}/\\d{1,}/\\d{2})\\s{1,}(\\d\\d:\\d\\d)") %>%
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(university = "Clemson University")

clemson <- clemson %>% 
  bind_rows(clemson_14_18)

clemson <- clemson %>% 
  group_by(across(-incident)) %>% 
  summarize(incident = paste(incident, collapse = " ")) %>% ungroup()

write_csv(clemson, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/clemson.csv")


