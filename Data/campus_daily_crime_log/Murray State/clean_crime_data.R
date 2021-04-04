

library(tidyverse)
library(lubridate)

load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Murray State/crime.rda")


murray_state <- final_crime %>% 
  rename("incident" = crime, "case_number" = incident_number, 
         "date_occurred" = date_of_incident, "time_occurred" = time_of_incident) %>% 
  mutate(university = "Murray State University") %>% 
  mutate(time_reported = gsub("\r\n", "", time_reported)) %>% 
  mutate(date_occurred = mdy(date_occurred), date_reported = mdy(date_reported)) 


write_csv(murray_state, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/murray_state.csv")
