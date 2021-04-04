#######################################################################################
##  This one I need to split the PDFs into halfs to get all the information correct  ##
#######################################################################################

library(tidyverse)
library(pdftools)
library(lubridate)
library(readxl)


### doing years 2013-2015 first
years <- c(2013:2015)

for (year in years){
  path <- paste("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/University of Idaho/",year,".xlsx", sep = "")
  name <- paste("idaho_", year, sep = '')
  data <- read_xlsx(path)
  data <- data %>% 
    select(-disposition) %>% 
    mutate(across(starts_with("time"), ~as.character(.)))
  assign(name, data)
}

## now year 2017 - format changed

path_2017 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/University of Idaho/2017.xlsx"

idaho_2017 <- read_xlsx(path_2017)
idaho_2017 <- idaho_2017 %>% 
  separate(date_reported, c("date_reported", "time_reported"), sep = "@") %>% 
  select(-day) %>% 
  mutate(date_occurred = ifelse(date_occurred == "Same", date_reported, date_occurred)) %>% 
  mutate(time_occurred = NA, location = NA) %>% 
  mutate(across(starts_with("time"), ~as.character(.)))

path_2018 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/University of Idaho/Master 2018 Campus Crime & Fire Log.txt"

idaho_2018 <- read_tsv(path_2018) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  janitor::clean_names()

idaho_2018 <- idaho_2018 %>% 
  select(-day_of_week, -master_id, -comments, - disposition, - clery_geography, - clery_offense, 
         -clery_count, - clery, -not_on_campus, - name_log) %>% 
  rename("date_reported" = date_time_reported, 
         "case_number" = mpd_case_number,
         "incident" = nature_of_calls,
         "location"= address_location,
         "date_occurred" = date_time_occurred) %>% 
  extract(date_reported, "time_reported", "(\\d\\d:\\d\\d)", remove = F) %>% 
  extract(date_reported, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d{2})") %>% 
  mutate(date_occurred = str_to_lower(date_occurred)) %>% 
  mutate(date_occurred = ifelse(date_occurred == "same", date_reported, date_occurred))  %>% 
  mutate(time_occurred = NA) %>% 
  mutate(across(starts_with("time"), ~as.character(.)))

path_2019 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/University of Idaho/Master 2019 Campus Crime & Fire Log.txt"

idaho_2019 <- read_tsv(path_2019) %>% 
  janitor::clean_names()

idaho_2019 <- idaho_2019[,1:8]

idaho_2019 <- idaho_2019 %>% 
  rename("date_reported" = date_time_reported, 
         "case_number" = mpd_case_number,
         "incident" = nature_of_calls,
         "location"= address_location,
         "date_occurred" = date_time_occurred) %>% 
  select(-comments, -day_of_week, - master_id) %>% 
  extract(date_reported, "time_reported", "(\\d\\d:\\d\\d)", remove = F) %>% 
  extract(date_reported, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d{2})") %>% 
  mutate(date_occurred = str_to_lower(date_occurred)) %>% 
  mutate(date_occurred = ifelse(date_occurred == "same", date_reported, date_occurred))  %>% 
  mutate(time_occurred = NA) %>% 
  mutate(across(starts_with("time"), ~as.character(.)))





#### 2016
path_2016 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/University of Idaho/2016_all.csv"
idaho_2016 <- read_csv(path_2016) %>% 
  janitor::clean_names()

idaho_2016 <- idaho_2016 %>% 
  rename("case_number" = mpd_number,
         "incident" = nature,
         "location" = address,
         "date_occurred" = date_of_occurrence,
         "time_occurred" = time_of_occurrence) %>% 
  select(-disposition) %>% 
  mutate(date_occurred = ifelse(date_occurred == "same", date_reported, date_occurred))  %>% 
  mutate(across(starts_with("time"), ~as.character(.)))
  

idaho <- bind_rows(idaho_2013,
                   idaho_2014,
                   idaho_2015,
                   idaho_2016,
                   idaho_2017,
                   idaho_2018,
                   idaho_2019)

idaho <- idaho %>%
  mutate(across(where(is.character), ~str_to_lower(.))) %>% 
  mutate(date_occurred = ifelse(date_occurred == "same", date_reported, date_occurred)) %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H:%M"), format = "%H:%M"))) %>% 
  mutate(university = "University of Idaho")


write_csv(idaho, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/idaho.csv")
