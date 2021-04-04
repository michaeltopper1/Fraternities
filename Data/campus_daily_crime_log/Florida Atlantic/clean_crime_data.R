######################################################################
##  This file cleans the extracted pdf data into my desired format  ##
######################################################################

library(tidyverse)
library(lubridate)

load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida Atlantic/cleaned_crime.rda")

florida_atlantic <- florida_atlantic %>% 
  mutate(date_occurred = mdy(date_occurred), date_reported = mdy(date_reported)) %>% 
  mutate(time_reported = NA) %>% 
  mutate(time_occurred = format(strptime(time_occurred,"%H:%M"), format = "%H:%M")) %>% 
  mutate(university = "Florida Atlantic University")

florida_atlantic <- florida_atlantic %>% 
  group_by(across(-incident)) %>% 
  summarize(incident = paste(incident, collapse = " ")) %>% 
  ungroup()


write_csv(florida_atlantic,
          file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/florida_atlantic.csv")
