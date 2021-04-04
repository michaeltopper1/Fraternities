
library(tidyverse)
library(lubridate)
library(readxl)

path_1 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/University of Missouri/CrimeLog_13-19.xlsx"
path_2 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/University of Missouri/CrimeLog19.xlsx"

missouri_1 <- read_xlsx(path_1) %>% 
  janitor::clean_names()
missouri_2 <- read_xlsx(path_2) %>% 
  janitor::clean_names()

missouri_1 <- missouri_1 %>% 
  select( -counts, -street_suffix, -apt, -disposition, -status_date) %>% 
  mutate(across(where(is.character), ~gsub("NULL", "",.))) %>% 
  mutate(location = str_trim(paste(house_number, street_prefix, street_name, city, state, zip))) %>% 
  select(-house_number, -street_prefix, -street_name, - city, -state, - zip) %>% 
  select(-starts_with("x"), -starts_with("street"), -occured_through_date, -crimal_offense) %>% 
  rename("date_reported"= report_date, "date_occurred" = occured_from_date,
         "incident" = incident_type) %>% 
  extract(date_reported, "time_reported", "(\\d\\d:\\d\\d:\\d\\d)", remove = F) %>% 
  extract(date_reported, "date_reported", "(\\d{4}-\\d\\d-\\d\\d)") %>% 
  extract(date_occurred, "time_occurred", "(\\d\\d:\\d\\d:\\d\\d)", remove = F) %>% 
  extract(date_occurred, "date_occurred", "(\\d{4}-\\d\\d-\\d\\d)") %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H:%M:%S"), format = "%H:%M"))) %>% 
  mutate(university = "University of Missouri-Columbia")

missouri_2 <- missouri_2 %>% 
  select(-chrgcnt, -chrgdesc, - descriptn, - date_status, - domestic, - dv_relation, -reportedas,
         -date_fnd) %>% 
  mutate(location = str_trim(paste(streetnbr, street, city, state, zip))) %>% 
  select(-streetnbr, -street, - city, -state, - zip, - apt_flr) %>% 
  rename("case_number" = inci_id, "incident" = offense, "date_reported" = date_report,
         "date_occurred" = date_occu) %>%
  extract(date_reported, "time_reported", "(\\d\\d:\\d\\d:\\d\\d)", remove = F) %>% 
  extract(date_reported, "date_reported", "(\\d{4}-\\d\\d-\\d\\d)") %>% 
  extract(date_occurred, "time_occurred", "(\\d\\d:\\d\\d:\\d\\d)", remove = F) %>% 
  extract(date_occurred, "date_occurred", "(\\d{4}-\\d\\d-\\d\\d)") %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H:%M:%S"), format = "%H:%M"))) %>% 
  mutate(university = "University of Missouri-Columbia") %>% 
  mutate(case_number = gsub("2019", "2019-", case_number))

missouri <- bind_rows(missouri_1, missouri_2)

write_csv(missouri, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/missouri.csv")         
