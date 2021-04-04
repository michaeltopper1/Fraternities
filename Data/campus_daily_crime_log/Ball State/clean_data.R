library(tidyverse)
library(lubridate)
library(readxl)

ball_14_15 <- read_csv("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Ball State/ball_2013_2015.csv")
ball_15_19 <- read_csv("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Ball State/ball_2015_2019.csv")

ball <- bind_rows(ball_14_15, ball_15_19) %>% 
  mutate(date_reported = mdy(date_reported)) %>% 
  mutate(time_reported = format(strptime(time_reported, "%H:%M"), format = "%H:%M"))

ball  <- ball %>% 
  group_by(across(-incident)) %>% 
  summarize(incident = paste(incident, collapse = " ")) %>% 
  ungroup()
  
write_csv(ball, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/ball.csv")
