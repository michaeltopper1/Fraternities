
library(tidyverse)
library(lubridate)
library(readxl)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Rollins/Copy of Crime and Fire Log for 01012015-01222021.xlsx"

rollins <- read_xlsx(path) %>% 
  janitor::clean_names()

rollins <- rollins %>% 
  rename("case_number" = c_s_report_number, "date_reported" = date_and_time_received,
         "incident" = call_classification) %>% 
  select(-clery_geography, -details, -disposition, -l_e_report_number, -student_conduct_report_number) %>% 
  extract(date_reported, "time_reported", "(\\d\\d:\\d\\d)", remove = F) %>% 
  extract(date_reported, "date_reported", "(\\d\\d\\d\\d-\\d\\d-\\d\\d)") %>% 
  mutate(date_reported = ymd(date_reported)) %>% 
  mutate(university = "Rollins College", time_occurred = NA, date_occurred = NA)


write_csv(rollins, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/rollins.csv")
