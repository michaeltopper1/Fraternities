
## Purpose of script: This will make a panel data set of all the dates for each of the schools I have cleaned
## crime data from. The main purpose of this is to merge together the crime data (collapsed) into the dates
## Author: Michael Topper
## Date Last Edited: 2021-03-22
##

library(tidyverse)
library(readxl)

## joins with IPEDS, creates university by month FE, creates university by year FE, and filters years greater than 2013.
create_data_analysis_week <- function(x) {
  data <- x %>% 
    left_join(ipeds, by = c("university" = "university", 'year' = 'year')) %>% 
    mutate(month = month(week), year = year(week)) %>% 
    mutate(across(c(sexual_assault, alcohol_offense,
                    theft, robbery_burglary, drug_offense, rape), list(ihs = ifc::ihs_transform),
                  .names = "{.fn}_{.col}")) %>% 
    group_by(university, month) %>% 
    mutate(uni_month = cur_group_id()) %>% 
    ungroup() %>% 
    mutate(across(c(sexual_assault, alcohol_offense,
                    theft, robbery_burglary, drug_offense, rape), ~./total_enrollment * 25000,
                  .names = '{.col}_per25')) %>% 
    group_by(university, year) %>% 
    mutate(uni_year = cur_group_id()) %>% 
    ungroup() %>% 
    filter(year > 2013)
  return(data)
}


## sourcing the clean_daily_crime_logs file which connects all of the cleaned crime logs togther
## and then creates new variables, and then collapses them
source("code/cleaning_crime_log_files/append_daily_crime_logs_1.R")



## this creates a short vector of the distinct universities in my cleaned crime data
universities <- appended_crime_logs %>% distinct(university) %>% pull(university)

## this takes all of the distinct universities and attaches a sequence of the years I am interested in and connects them together
## this is what I am using to attach the collapsed crime data to
## school_dates is the final product: a tibble of just dates and schools from 2013-2019 by-day
for (i in seq_along(universities)){
  date_data <- tibble("date" = seq(from = ymd('2014-01-01'), to= ymd('2019-12-31'), by = 'days'))
  if (i == 1) {
    school_dates <- date_data %>% 
      mutate(university = universities[i])
  }
  else {
    new_data <- date_data %>% 
      mutate(university = universities[i])
    school_dates <- school_dates %>% rbind(new_data)
  }
}

## Next Step: Now I need to merge the two datas together - the collapsed data and the large panel.

## joining together the dates and the counts to make a long panel
daily_panel <- school_dates %>% 
  left_join(collapsed_data_daily, by = c("university" = "university", "date" = "date_preferred")) %>% 
  mutate(year = year(date), month = month(date), day = day(date))




## now I need to pull in data that keeps track of which years I have missing
missing_years <- readxl::read_xlsx("data/campus_daily_crime_log/crime_log_list.xlsx",
                           sheet = "missing_years") %>% janitor::clean_names()


## creates a list of schools missing year 2014
not_missing_2014 <- missing_years %>% 
  filter(missing_2014 == 0) %>% 
  pull(university)


## replaces all days with NAs with 0s if i had data for that year
## i did this because i can assume there were 0 reports on a date that there was no crime in years i have
daily_panel <- daily_panel %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape"), 
                ~ifelse(year== 2014 & is.na(.) & university %in% not_missing_2014,
                        0, .))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape"), 
                ~ifelse(year >= 2015 & is.na(.), 0, .)))## changes all missings from 2015 onwards to 0s- careful of North Florida!

## North Florida I only have information from July 2015- onwards. Here I am accounting for this

daily_panel <- daily_panel %>% 
  mutate(weekday = wday(date, label = T))


## Ferrum College is missing data. only have september 2015 - 2019
## Delaware state only has data from 2017 - onwards
## Texas Austin is missing jan/feb 2016
## NC state is missing data from August 2014 and before
## UCSB missing 2019 and december 2018
## James Madison university only 2017-2019
## ALbany state only 2017-2019
daily_panel <- daily_panel %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape"), ~ifelse(
    year == 2015 & month < 9 & university == "Ferrum College",
    NA, .
  ))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape"), ~ifelse(
    year <= 2016 & university == "Delaware State University",
    NA, .
  ))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape"), ~ifelse(
    year== 2016 & (month == 1 | month == 2) & university == "The University of Texas at Austin",
    NA, .
  ))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape"), ~ifelse(
    year== 2014 & (month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6 | month == 7| month == 8) & university == "North Carolina State University at Raleigh",
    NA, .
  ))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape"), ~ifelse(
    year== 2018 & (month == 12) & university == "University of California-Santa Barbara",
    NA, .
  ))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape"), ~ifelse(
    year== 2019 & university == "University of California-Santa Barbara",
    NA, .
  ))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape"), ~ifelse(
    year <= 2016 & university == "James Madison University",
    NA, .
  ))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape"), ~ifelse(
    year <= 2016 & university == "Albany State University",
    NA, .
  )))



##### Last Step: pulling in the closure data and merging it with this final_panel
closures <- readxl::read_xlsx("data/closure_spreadsheet_final_2019.xlsx") %>% 
  janitor::clean_names() %>% 
  select(university, date, deadline, date2, deadline2,
         university_enacted_1, university_enacted_2, reason1, reason2) %>% 
  rename("closure_1" = date,
         "closure_1_end" = deadline,
         "closure_2" = date2,
         "closure_2_end" = deadline2)



daily_panel <- daily_panel %>% 
  left_join(closures, by = c("university" = "university")) %>% 
  mutate(treatment = case_when(
    (!is.na(closure_1) & !is.na(closure_1_end)) & (date >= closure_1 & date < closure_1_end) ~ 1,
    (!is.na(closure_2) & !is.na(closure_2_end)) & (date >= closure_2 & date < closure_2_end) ~ 1,
    TRUE ~as.double(0)
  )) %>% 
  mutate(university_enacted = case_when(
    university_enacted_1 == 1 & treatment == 1 ~ 1,
    university_enacted_2 == 1 & treatment == 1 ~ 1,
    TRUE ~as.double(0)
  ))



## Weekly panel
weekly_panel <- daily_panel %>% 
  group_by(university, week = cut(date, "week")) %>% 
  summarize(across(c(sexual_assault,
                     alcohol_offense,
                     drug_offense,
                     theft,
                     robbery_burglary,
                     alcohol_offense_strict,
                     noise_offense,
                     treatment,
                     rape), ~ sum(., na.rm  = T))) %>% 
  mutate(week = ymd(week)) %>% 
  mutate(week_id = wday(week, label = T)) %>% 
  ungroup() 


## yearly panel:
yearly_panel <- daily_panel %>% 
  group_by(year, university) %>% 
  summarize(sexual_assault = sum(sexual_assault),
            alcohol_offense = sum(alcohol_offense),
            drug_offense = sum(drug_offense),
            theft = sum(theft),
            robbery_burglary = sum(robbery_burglary),
            alcohol_offense_strict = sum(alcohol_offense_strict),
            noise_offense = sum(noise_offense),
            treatment_day = sum(treatment),
            treatment = mean(treatment),
            rape = sum(rape)) %>% 
  ungroup() %>% arrange(university, year) 










### adding in IPEDS data
ipeds <- read_csv("created_data/ipeds/ipeds_final.csv") %>% 
  filter(year > 2013)




daily_panel <-  daily_panel %>% 
 left_join(ipeds, by = c("university" = "university", 'year' = 'year')) %>% 
  mutate(across(c(sexual_assault, alcohol_offense,
                  theft, robbery_burglary, drug_offense, rape), list(ihs = ifc::ihs_transform),
                .names = "{.fn}_{.col}")) %>% 
  group_by(university, month) %>% 
  mutate(uni_month = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(across(c(sexual_assault, alcohol_offense,
                  theft, robbery_burglary, drug_offense, rape), ~./total_enrollment * 25000,
                .names = '{.col}_per25')) %>% 
  group_by(university, year) %>% 
  mutate(uni_year = cur_group_id()) %>% 
  ungroup() %>% 
  filter(year > 2013)


## yearly panel:
yearly_panel <- daily_panel %>% 
  group_by(year, university) %>% 
  summarize(sexual_assault = sum(sexual_assault),
            alcohol_offense = sum(alcohol_offense),
            drug_offense = sum(drug_offense),
            theft = sum(theft),
            robbery_burglary = sum(robbery_burglary),
            alcohol_offense_strict = sum(alcohol_offense_strict),
            noise_offense = sum(noise_offense),
            treatment_day = sum(treatment),
            treatment = mean(treatment),
            rape = sum(rape)) %>%
  ungroup() %>% arrange(university, year) 





yearly_panel <- yearly_panel %>% 
  left_join(ipeds, by= c("university" = "university", 'year' = "year"))


## writing yearly panel
write_csv(yearly_panel, file = "created_data/xmaster_data/yearly_panel_full_calendar.csv")
