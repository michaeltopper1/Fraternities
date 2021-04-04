
library(tidyverse)
library(lubridate)


load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Miami University/total_crime.rda")


miami <- total_crime %>% 
  separate(date_occurred, into = c("date_occurred", "other"), sep = "and") %>% 
  mutate(date_occurred = str_trim(date_occurred), other = str_trim(other)) %>% 
  select(-other) %>% 
  extract(date_occurred, into = "time_occurred", regex = "(\\d\\d\\d\\d$)", remove = F) %>% 
  extract(date_occurred, into = "date_occurred", regex = "(\\d{1,}/\\d{1,}/\\d\\d\\d\\d)") %>% 
  extract(date_reported, into = "time_reported", regex = "(\\d\\d\\d\\d$)", remove = F) %>% 
  extract(date_reported, into = "date_reported", regex = "(\\d{1,}/\\d{1,}/\\d\\d\\d\\d)") %>% 
  mutate(time_reported = format(strptime(time_reported, format = "%H%M"), format = "%H:%M")) %>% 
  mutate(time_occurred = format(strptime(time_occurred, format = "%H%M"), format = "%H:%M")) %>% 
  rename("incident" = crime) %>% 
  mutate(university = "Miami University-Oxford", location = NA) %>% 
  mutate(date_reported= mdy(date_reported), date_occurred = mdy(date_occurred))


write_csv(miami, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/miami.csv")