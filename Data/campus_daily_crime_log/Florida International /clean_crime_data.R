###############################################################################################################################
##  This file cleans all data from florida international and appends it. It outputs to csv in separate folder::cleaned_data  ##
###############################################################################################################################


library(tidyverse)
library(lubridate)

years <- c(2015:2019)
months <- c("01", "02", "03",
            "04", "05", "06",
            "07", "08", "09",
            "10", "11", "12")

## concatenating together all the years. 
for (year in years) {
  print(year)
  for (month in months){
    print(month)
    if (month == "01" & year == 2015) {
      florida_international_pages <- read_csv(
        paste("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida International /"
              ,year, "_",month,"-1.csv", sep = ""), col_names = F)
    }
    else {
      florida_i_append <- read_csv(
        paste("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida International /",
              year, "_",month,"-1.csv", sep = ""), col_names = F)
        florida_international_pages <- florida_international_pages %>% 
          bind_rows(florida_i_append)
    }
    }
  }

## getting rid of the columns that I don't need and getting rid of NA incidents
florida_international_pages <- florida_international_pages %>% 
  select(-X8, -X5, -X7) %>% 
  rename("incident" = X1, 
         "case_number" = X2, 
         "date_reported" = X3, 
         "date_occurred" = X4, "location" = X6) %>% 
  filter(!is.na(incident) & !is.na(case_number))

## extracting times and changing to military time, in addition to changing date format.
florida_international_pages <- florida_international_pages %>% 
  extract(date_reported, into = c("time_reported"),
          regex= "(\\d\\d:\\d\\d\\s[AP])", remove = F) %>% 
  extract(date_reported, into = "date_reported", regex = "(\\d\\d/\\d\\d/\\d\\d\\d\\d)") %>% 
  extract(date_occurred, into = "time_occurred", regex = "(\\d\\d:\\d\\d\\s[AP])", remove = F) %>% 
  extract(date_occurred, into = "date_occurred", regex = "(\\d\\d/\\d\\d/\\d\\d\\d\\d)") %>% 
  mutate(time_reported = paste(time_reported, "M", sep = "")) %>% 
  mutate(time_occurred = paste(time_occurred, "M", sep = "")) %>% 
  mutate(time_occurred = format(strptime(time_occurred,"%I:%M %p"), format = "%H:%M")) %>% 
  mutate(time_reported = format(strptime(time_reported,"%I:%M %p"), format = "%H:%M")) %>% 
  mutate(date_reported = mdy(date_reported), date_occurred = mdy(date_occurred)) 

## florida_international pages are cleaned. 
florida_international_pages_cleaned <- florida_international_pages

## loading in 2013/14 
load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida International /crime_201314.rda")

florida_i_2013_2014_cleaned <- crime_1314 %>% 
  janitor::clean_names() %>% 
  select(-disposition, -x8, - campus) %>% 
  rename("incident" = type_of_crime, "date_reported" = date_time_reported,
         "date_occurred" = date_time_occurred) %>% 
  extract(date_reported, into = c("time_reported"),
          regex= "(\\d\\d:\\d\\d\\s[AP])", remove = F) %>% 
  extract(date_reported, into = "date_reported", regex = "(\\d\\d/\\d\\d/\\d\\d\\d\\d)") %>% 
  extract(date_occurred, into = "time_occurred", regex = "(\\d\\d:\\d\\d\\s[AP])", remove = F) %>% 
  extract(date_occurred, into = "date_occurred", regex = "(\\d\\d/\\d\\d/\\d\\d\\d\\d)") %>% 
  mutate(time_reported = paste(time_reported, "M", sep = "")) %>% 
  mutate(time_occurred = paste(time_occurred, "M", sep = "")) %>% 
  mutate(time_occurred = format(strptime(time_occurred,"%I:%M %p"), format = "%H:%M")) %>% 
  mutate(time_reported = format(strptime(time_reported,"%I:%M %p"), format = "%H:%M")) %>% 
  mutate(date_reported = mdy(date_reported), date_occurred = mdy(date_occurred)) 

## loading in 2015/2019
load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida International /crime_201519.rda")

florida_i_2015_2019_cleaned <- crime_table_2 %>% 
  select(-V5, -V7) %>% 
  rename("incident" = V1, 
          "case_number" = V2, 
          "date_reported" = V3, 
          "date_occurred" = V4, "location" = V6) %>% 
  extract(date_reported, into = c("time_reported"),
          regex= "(\\d\\d:\\d\\d\\s[AP])", remove = F) %>% 
  extract(date_reported, into = "date_reported", regex = "(\\d\\d/\\d\\d/\\d\\d\\d\\d)") %>% 
  extract(date_occurred, into = "time_occurred", regex = "(\\d\\d:\\d\\d\\s[AP])", remove = F) %>% 
  extract(date_occurred, into = "date_occurred", regex = "(\\d\\d/\\d\\d/\\d\\d\\d\\d)") %>% 
  mutate(time_reported = paste(time_reported, "M", sep = "")) %>% 
  mutate(time_occurred = paste(time_occurred, "M", sep = "")) %>% 
  mutate(time_occurred = format(strptime(time_occurred,"%I:%M %p"), format = "%H:%M")) %>% 
  mutate(time_reported = format(strptime(time_reported,"%I:%M %p"), format = "%H:%M")) %>% 
  mutate(date_reported = mdy(date_reported), date_occurred = mdy(date_occurred))

florida_international <- bind_rows(florida_i_2013_2014_cleaned, florida_i_2015_2019_cleaned,
                                   florida_international_pages_cleaned)

florida_international <- florida_international %>% 
  mutate(university = "Florida International University")

write_csv(florida_international, file =
            "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/florida_international.csv")
