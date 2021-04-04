
library(tidyverse)
library(lubridate)
library(readxl)

path_2013 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Monmouth/fwexternaldailycrimerecordsrequest/2013 Crime Log.csv"

monmouth_2013 <- read_csv(path_2013, skip = 2)
monmouth_2013 <- monmouth_2013[,1:15] %>% 
  janitor::clean_names() %>% 
  rename("date_occurred" = date_of_occurrence,
         "location" = location_of_incident, "incident"= crime_reported) %>% 
  select(-x7, -officer, -x12, -detective, -case_status, -remarks, -x9, -x13)

monmouth_2013 <- monmouth_2013 %>% 
  mutate(across(where(is.character), ~str_to_lower(.))) %>% 
  mutate(date_occurred = str_trim(gsub("b/w", "", date_occurred))) %>% 
  mutate(time_occurred = str_trim(gsub("b/w", "", time_occurred))) %>% 
  mutate(date_occurred = gsub("2013", "13", date_occurred)) %>% 
  extract(date_occurred, into = "date_occurred", regex = "(^\\d{1,2}/\\d{1,2}/\\d{1,2})") %>% 
  extract(time_occurred, into = "time_occurred", regex = "(^\\d{1,4})") %>% 
  filter(!is.na(incident))

monmouth_2013 <- monmouth_2013 %>% 
  mutate(time_occurred = paste("00", time_occurred, sep = "")) %>% 
  extract(time_occurred, into = "time_occurred", regex = "(\\d\\d\\d\\d$)") %>% 
  mutate(time_reported = paste("00", time_reported, sep = "")) %>% 
  extract(time_reported, into = "time_reported", regex = "(\\d\\d\\d\\d$)") %>% 
  mutate(time_reported = format(strptime(time_reported, format = "%H%M"), format = "%H:%M")) %>% 
  mutate(time_occurred = format(strptime(time_occurred, format = "%H%M"), format = "%H:%M")) %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(university = "Monmouth University")


path_2014 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Monmouth/fwexternaldailycrimerecordsrequest/2014 Crime Log.csv"

monmouth_2014 <- read_csv(path_2014, skip = 3)[,1:15] %>% 
  janitor::clean_names() %>% 
  rename("date_occurred" = date_of_occurrence,
         "location" = location_of_incident, "incident"= crime_reported) %>% 
  select(-x7, -officer, -x12, -detective, -case_status, -remarks, -x9, -x13)

monmouth_2014 <- monmouth_2014 %>% 
  mutate(across(where(is.character), ~str_to_lower(.))) %>% 
  mutate(across(starts_with("time"), ~str_trim(gsub("b/w", "",.)))) %>% 
  mutate(date_occurred = str_trim(gsub("b/w", "", date_occurred))) %>% 
  extract(time_occurred, into = "time_occurred", regex = "(^\\d{1,4})") %>%
  extract(date_occurred, into = "date_occurred", regex = "(^\\d{1,2}/\\d{1,2}/\\d{1,2})") %>% 
  mutate(across(starts_with("time"), ~paste("00", ., sep = ""))) %>% 
  extract(time_occurred, "time_occurred", regex = "(\\d\\d\\d\\d$)") %>% 
  extract(time_reported, "time_reported", regex = "(\\d\\d\\d\\d$)") %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) %>% 
  mutate(date_reported = gsub("/\\d{3,}", "/14", date_reported)) %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(university = "Monmouth University")


path_2015 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Monmouth/fwexternaldailycrimerecordsrequest/2015 Crime Log.csv"

monmouth_2015 <- read_csv(path_2015, skip = 2)[,1:8] %>% 
  janitor::clean_names() %>% 
  rename("date_occurred" = date_of_occurrence,
                                    "location" = location_of_incident, "incident"= crime_reported) %>% 
  select(-x7)


monmouth_2015 <- monmouth_2015 %>% 
  mutate(across(where(is.character), ~str_to_lower(.))) %>% 
  mutate(across(starts_with("time"), ~str_trim(gsub("b/w", "",.)))) %>% 
  mutate(date_occurred = str_trim(gsub("b/w", "", date_occurred))) %>% 
  extract(time_occurred, into = "time_occurred", regex = "(^\\d{1,4})") %>%
  extract(date_occurred, into = "date_occurred", regex = "(^\\d{1,2}/\\d{1,2}/\\d{1,2})") %>% 
  mutate(across(starts_with("time"), ~paste("00", ., sep = ""))) %>% 
  extract(time_occurred, "time_occurred", regex = "(\\d\\d\\d\\d$)") %>% 
  extract(time_reported, "time_reported", regex = "(\\d\\d\\d\\d$)") %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) %>% 
  mutate(date_reported = gsub("/\\d{3,}", "/15", date_reported)) %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(university = "Monmouth University") 

path_2016 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Monmouth/fwexternaldailycrimerecordsrequest/2016 Crime Log.csv"

monmouth_2016 <- read_csv(path_2016, skip  = 2)[,1:8] %>% 
  janitor::clean_names() %>% 
  rename("date_occurred" = date_of_occurrence,
         "location" = location_of_incident, "incident"= crime_reported) %>% 
  select(-x7)

monmouth_2016 <- monmouth_2016 %>% 
  mutate(across(where(is.character), ~str_to_lower(.))) %>% 
  mutate(across(starts_with("time"), ~str_trim(gsub("b/w", "",.)))) %>% 
  mutate(date_occurred = str_trim(gsub("b/w", "", date_occurred))) %>% 
  extract(time_occurred, into = "time_occurred", regex = "(^\\d{1,4})") %>%
  extract(date_occurred, into = "date_occurred", regex = "(^\\d{1,2}/\\d{1,2}/\\d{1,2})") %>% 
  mutate(across(starts_with("time"), ~paste("00", ., sep = ""))) %>% 
  extract(time_occurred, "time_occurred", regex = "(\\d\\d\\d\\d$)") %>% 
  extract(time_reported, "time_reported", regex = "(\\d\\d\\d\\d$)") %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) %>% 
  mutate(date_reported = gsub("/\\d{3,}", "/16", date_reported)) %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(university = "Monmouth University") 

path_2017 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Monmouth/fwexternaldailycrimerecordsrequest/2017 Crime Log.csv"

monmouth_2017 <- read_csv(path_2017)[,1:8] %>% 
  janitor::clean_names() %>% 
  select(-specific_location) %>% 
  rename("date_occurred" = date_of_occurrence,
         "location" = location_of_incident, "incident"= crime_reported)

monmouth_2017 <- monmouth_2017 %>% 
  mutate(across(where(is.character), ~str_to_lower(.))) %>% 
  mutate(across(starts_with("time"), ~str_trim(gsub("b/w", "",.)))) %>% 
  mutate(date_occurred = str_trim(gsub("b/w", "", date_occurred))) %>% 
  extract(time_occurred, into = "time_occurred", regex = "(^\\d{1,4})") %>%
  extract(date_occurred, into = "date_occurred", regex = "(^\\d{1,2}/\\d{1,2}/\\d{1,2})") %>% 
  mutate(across(starts_with("time"), ~paste("00", ., sep = ""))) %>% 
  extract(time_occurred, "time_occurred", regex = "(\\d\\d\\d\\d$)") %>% 
  extract(time_reported, "time_reported", regex = "(\\d\\d\\d\\d$)") %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) %>% 
  mutate(date_reported = gsub("/\\d{3,}", "/17", date_reported)) %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(university = "Monmouth University") 

path_2018 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Monmouth/fwexternaldailycrimerecordsrequest/2018 Crime Log.csv"

monmouth_2018 <- read_csv(path_2018)[,1:8] %>% 
  janitor::clean_names() %>% 
  select(-specific_location) %>% 
  rename("date_occurred" = date_of_occurrence,
         "location" = location_of_incident, "incident"= crime_reported)%>% 
  mutate(across(where(is.character), ~str_to_lower(.))) %>% 
  mutate(across(starts_with("time"), ~str_trim(gsub("b/w", "",.)))) %>% 
  mutate(date_occurred = str_trim(gsub("b/w", "", date_occurred))) %>% 
  extract(time_occurred, into = "time_occurred", regex = "(^\\d{1,4})") %>%
  extract(date_occurred, into = "date_occurred", regex = "(^\\d{1,2}/\\d{1,2}/\\d{1,2})") %>% 
  mutate(across(starts_with("time"), ~paste("00", ., sep = ""))) %>% 
  extract(time_occurred, "time_occurred", regex = "(\\d\\d\\d\\d$)") %>% 
  extract(time_reported, "time_reported", regex = "(\\d\\d\\d\\d$)") %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) %>% 
  mutate(date_reported = gsub("/\\d{3,}", "/18", date_reported)) %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(university = "Monmouth University") 

path_2019 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Monmouth/fwexternaldailycrimerecordsrequest/2019 Crime Log.csv"

monmouth_2019 <- read_csv(path_2019)[,1:8] %>% 
  janitor::clean_names() %>% 
  select(-specific_location) %>% 
  rename("date_occurred" = date_of_occurrence,
         "location" = location_of_incident, "incident"= crime_reported)%>% 
  mutate(across(where(is.character), ~str_to_lower(.))) %>% 
  mutate(across(starts_with("time"), ~str_trim(gsub("b/w", "",.)))) %>% 
  mutate(date_occurred = str_trim(gsub("b/w", "", date_occurred))) %>% 
  extract(time_occurred, into = "time_occurred", regex = "(^\\d{1,4})") %>%
  extract(date_occurred, into = "date_occurred", regex = "(^\\d{1,2}/\\d{1,2}/\\d{1,2})") %>% 
  mutate(across(starts_with("time"), ~paste("00", ., sep = ""))) %>% 
  extract(time_occurred, "time_occurred", regex = "(\\d\\d\\d\\d$)") %>% 
  extract(time_reported, "time_reported", regex = "(\\d\\d\\d\\d$)") %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M"))) %>% 
  mutate(date_reported = gsub("/\\d{3,}", "/19", date_reported)) %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(university = "Monmouth University") 

monmouth <- bind_rows(monmouth_2013, monmouth_2014,
                      monmouth_2015, monmouth_2016,
                      monmouth_2017, monmouth_2018,
                      monmouth_2019) %>% filter(!is.na(incident))


write_csv(monmouth, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/monmouth.csv")
