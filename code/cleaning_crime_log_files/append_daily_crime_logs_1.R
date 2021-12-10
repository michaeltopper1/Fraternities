## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-07
##

library(tidyverse)
library(lubridate)

directory = "data/campus_daily_crime_log/cleaned_schools"
files <- list.files(directory)
files <- map(files, ~paste0("data/campus_daily_crime_log/cleaned_schools/", .))

## binds all data together
appended_crime_logs <- map_df(files,
                  ~ read_csv(.,col_types = cols(
                        time_occurred = col_character(),
                        time_reported = col_character(),
                        location = col_character(),
                        case_number = col_character(),
                        date_reported = col_date(),
                        date_occurred = col_date(),
                        incident = col_character(),
                        university = col_character()
                      ), guess_max = 2000
                  ))


## creating specific year-month-day columns
appended_crime_logs <- appended_crime_logs %>% 
  mutate(date_preferred  = lubridate::as_date(ifelse(!is.na(date_occurred), date_occurred, date_reported))) %>% 
  mutate(year_preferred = year(date_preferred),
         month_preferred = month(date_preferred),
         day_preferred = day(date_preferred)) %>% 
  mutate(year_reported = year(date_reported),
         month_reported = month(date_reported),
         day_reported = day(date_reported)) %>% 
  mutate(year_occurred = year(date_occurred),
         month_occurred = month(date_occurred),
         day_occurred = day(date_occurred)) %>% 
  extract(time_reported, "hour_reported", "(^\\d\\d)", remove = F) %>% 
  extract(time_occurred, "hour_occurred", "(^\\d\\d)", remove = F) 


## this gets all the data to be unique
appended_crime_logs <- appended_crime_logs %>% 
  group_by(across(-incident)) %>% 
  summarize(incident = paste(incident, collapse = " ")) %>% 
  ungroup()
## taking only the distinct rows across my 8 columns. 
# appended_crime_logs <- appended_crime_logs %>% 
#   mutate(case_number = ifelse(case_number == "N/A", NA, case_number)) %>% 
#   group_by(university, case_number, date_reported, incident, time_reported, date_occurred, location, time_occurred) %>% 
#   distinct() %>% 
#   ungroup()

## changing incidents to lower
## changing hours columns to doubles
appended_crime_logs <- appended_crime_logs %>% 
  mutate(across(starts_with("incident"), ~str_to_lower(.))) %>% 
  mutate(across(starts_with("hour"), ~as.double(.)))

## filtering out any date reported that are NA or incidents that are NA - this data is not useful to me
appended_crime_logs <- appended_crime_logs %>% 
  filter(!is.na(date_preferred)) %>% 
  filter(!is.na(incident))


## got theft from -https://ucr.fbi.gov/nibrs/2012/resources/nibrs-offense-definitions
## update these in the matching_word_table.R document if you change them. I no longer pull from here
alcohol_identifiers <- "alcohol|dwi|intox|drink|dui|drunk|liquor|driving under the influence|dip|abcc|underage|dwi|underage|pula|owi|mip|under age|beer|wine|booze|minor in possession|ovi" ## got rid of disorderly conduct.
sexual_assault_identifiers <- "sex|rape|fondling|fondle" 
drug_identifiers <- "drug|narcotic|marijuana|heroin|overdose|cocaine|controlled substance"
theft_identifiers <- "larceny|theft|shoplifting|pocket-picking|steal|shop lifting" ##using nibrs
robbery_burglary_identifiers <- "robbery|burglary|unlawful entry|breaking and entering"
alcohol_identifiers_strict <- "alcohol|dwi|intox|drink|dui|drunk|liquor|driving under the influence|dip|abcc|underage|beverage|dwi|underage|container|pula|owi|mip|under age|minor in possession|ovi" ## getting rid of possesion
noise_violation_identifier <- "noise|loud"
rape_identifier <- "rape"


## creating the variables for drug, alcohol, and sex offenses
## change the regular expressions if you want to modify these definitions
appended_crime_logs <- appended_crime_logs %>% 
  mutate(alcohol_offense = 
           ifelse(str_detect(incident,
                             alcohol_identifiers), 1, 0)) %>% 
  mutate(sexual_assault = ifelse(
    str_detect(incident, sexual_assault_identifiers), 1, 0)
  ) %>% 
  mutate(drug_offense = ifelse(
    str_detect(incident, drug_identifiers), 1, 0
  )) %>% 
  mutate(theft = ifelse(
    str_detect(incident, theft_identifiers), 1, 0
  )) %>% 
  mutate(robbery_burglary = ifelse(
    str_detect(incident, robbery_burglary_identifiers), 1, 0
  )) %>% 
  mutate(alcohol_offense_strict  = 
           ifelse(str_detect(incident, alcohol_identifiers_strict),1, 0)) %>% 
  mutate(noise_offense = 
           ifelse(str_detect(incident, noise_violation_identifier),1 , 0)) %>% 
  mutate(rape = 
           ifelse(str_detect(incident, rape_identifier), 1, 0))

appended_crime_logs <- appended_crime_logs %>% 
  mutate(sexual_assault = ifelse(sexual_assault ==1 & str_detect(incident,"orientation"), 0, sexual_assault)) %>% 
  mutate(year_occurred = lubridate::year(date_occurred), year_reported = lubridate::year(date_reported)) %>% 
  filter(year_reported > 2013 ) %>% 
  filter(year_occurred > 2013 | is.na(year_occurred)) %>% 
  filter(year_reported < 2020) %>% 
  filter(year_occurred <2020 | is.na(year_occurred))
  


appended_crime_logs <- appended_crime_logs %>% 
  mutate(report_lag = date_reported - date_occurred) %>% relocate(report_lag) %>% 
  filter(report_lag >=0 | is.na(report_lag)) %>% 
  mutate(report_lag_alc = ifelse(report_lag > 3 & alcohol_offense == 1, 1, 0)) %>% 
  mutate(report_lag_sex = ifelse(report_lag > 3 & sexual_assault == 1, 1, 0)) %>% 
  mutate(report_lag_drug = ifelse(report_lag > 3 & drug_offense == 1, 1, 0)) %>% 
  mutate(report_lag_rob = ifelse(report_lag > 3 & robbery_burglary == 1, 1, 0)) %>% 
  mutate(report_lag_theft = ifelse(report_lag > 3 & theft == 1, 1, 0))


write_csv(appended_crime_logs, file = "created_data/xmaster_data/appended_crime_logs.csv")



collapsed_data_daily <- appended_crime_logs %>% 
  select(date_preferred, university, sexual_assault, alcohol_offense, drug_offense, theft, robbery_burglary, alcohol_offense_strict,
         noise_offense, rape, starts_with("report_lag_")) %>% 
  group_by(university, date_preferred) %>% 
  summarize(across(everything(), ~sum(.,na.rm = T)))









