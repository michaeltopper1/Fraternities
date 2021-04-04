
library(tidyverse)
library(lubridate)
library(readxl)


path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/San Diego State/Daily Crime & Fire Log.xls"

sdsu <- read_xls(path)

sdsu <- sdsu %>% 
  janitor::clean_names() %>%
  rename("date_reported" = reported,
         "date_occurred" = occurred,
         "incident" = nature) 

sdsu <- sdsu %>% 
  mutate(across(-incident, ~zoo::na.locf(.))) %>% 
  extract(date_reported, c("date_reported", "time_reported"), "(\\d{1,2}/\\d{1,2}/\\d{4}).{1,}(\\d{1,2}:\\d{1,2})") %>% 
  extract(date_occurred, c("date_occurred", "time_occurred"), "(\\d{1,2}/\\d{1,2}/\\d{4}).{1,}(\\d{1,2}:\\d{1,2})") %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H:%M"), format = "%H:%M"))) %>% 
  select(-disposition, -on_campus) %>% 
  mutate(university = "San Diego State University")

sdsu <- sdsu %>% 
  group_by(across(-incident)) %>% 
  summarize(incident = paste(incident, collapse = " ")) %>% 
  ungroup()

write_csv(sdsu, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/sdsu.csv")




