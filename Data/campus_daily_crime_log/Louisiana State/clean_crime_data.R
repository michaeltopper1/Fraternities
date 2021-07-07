
library(tidyverse)
library(lubridate)
library(glue)

## loading in all the data
crime_2014 <- read_csv("Data/campus_daily_crime_log/Louisiana State/2014.csv")
crime_2015 <- read_csv("Data/campus_daily_crime_log/Louisiana State/2015.csv")
crime_2016 <- read_csv("Data/campus_daily_crime_log/Louisiana State/2016.csv")
crime_2017_2019 <- read_csv("Data/campus_daily_crime_log/Louisiana State/2017_2019.csv")

## binding together the data
louisiana_state <- bind_rows(
  crime_2014, crime_2015,
  crime_2016, crime_2017_2019
)

## first cleaning the column names and getting only the columns I need
louisiana_state <- louisiana_state  %>% 
  mutate(time_reported = NA, university = "Louisiana State University and Agricultural & Mechanical College",
         time_occurred = NA, location = NA)
  

  

write_csv(louisiana_state, file = "Data/campus_daily_crime_log/Cleaned_schools/louisiana_state.csv")
