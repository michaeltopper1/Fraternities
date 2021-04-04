
library(tidyverse)
library(lubridate)

years <- c(2014:2019)

for (year in years) {
  load(paste("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Indiana University/crime_",
             year,".rda", sep = ""))
}

indiana <- bind_rows(crime_total, 
                     crime_total_2015,
                     crime_total_2016,
                     crime_total_2017,
                     crime_total_2018,
                     crime_total_2019)
indiana %>% 
  select(-value) %>% 
  mutate(university = "Indiana University-Bloomington") %>% 
  rename("date_reported" = reported, "time")
  
