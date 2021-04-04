## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-25
##

library(tidyverse)
library(lubridate)

path_13_17 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Michigan/michigan_13_17.csv"
michigan_13_17 <- read_csv(path_13_17) ## errors are just for lat and long which i don't need

path_18_19 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Michigan/michigan_18_19.csv"
michigan_18_19 <- read_csv(path_18_19)

michigan <- list(michigan_13_17, michigan_18_19)

michigan <- map(michigan, ~rename(., "case_number" = id,
                            "date_reported" = date,
                            "incident" = description) %>% 
      mutate(university = "University of Michigan-Ann Arbor") %>% 
      extract(date_reported, c('date_reported', 'time_reported'), "(\\d{4}-\\d{1,2}-\\d{1,2})\\s(\\d{1,2}:\\d{1,2})") %>% 
      mutate("date_occurred" = NA, "time_occurred" = NA) %>% 
      mutate(date_reported = ymd(date_reported)) %>% 
      select(-disposition, -narrative, -status, -latitude, -longitude, -address)) %>% 
   reduce(bind_rows)

michigan <- michigan %>% 
  select(-arrest)

write_csv(michigan, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/michigan.csv")
