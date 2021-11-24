## Purpose of script: replaces the create_panel_2 document 
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-15
##

library(tidyverse)
library(readxl)
library(haven)



# Sourcing the previous script to get counts ------------------------------

## sourcing the clean_daily_crime_logs file which connects all of the cleaned crime logs togther
## and then creates new variables, and then collapses them
source("code/cleaning_crime_log_files/append_daily_crime_logs_1.R")



## this creates a short vector of the distinct universities in my cleaned crime data
universities <- appended_crime_logs %>% distinct(university) %>% pull(university)





# Creating Academic Calendar panels by semester ---------------------------

## pulling in the academic calendars
academic_calendars <- read_csv("data/academic_calendars.csv") %>% janitor::clean_names()

## moving all calendar dates to start in 2014
academic_calendars <- academic_calendars %>% 
  extract(fall_start, "fall_start", "(\\d{1,2}/\\d{1,2})") %>% 
  mutate(fall_start = paste0(fall_start, "/", "14")) %>% 
  extract(fall_end, "fall_end", "(\\d{1,2}/\\d{1,2})") %>% 
  mutate(fall_end = paste0(fall_end, "/", "14")) %>% 
  extract(spring_start, "spring_start", "(\\d{1,2}/\\d{1,2})") %>% 
  mutate(spring_start = paste0(spring_start, "/", "14")) %>% 
  extract(spring_end, "spring_end", "(\\d{1,2}/\\d{1,2})") %>% 
  mutate(spring_end = paste0(spring_end, "/", "14")) %>% 
  mutate(across(c(-university), ~mdy(.))) 

## adding 1 week to the fall_start of the academic calendar to account for welcome week.
## adding 1 week to the end of spring for conservative ness
academic_calendars <- academic_calendars %>% 
  mutate(fall_start = fall_start - days(7), spring_end = spring_end + days(7)) %>% 
  mutate(spring_start = case_when(
    spring_start == mdy("1/7/14") ~mdy("1/2/14"),
    spring_start == mdy("1/6/14") ~mdy("1/2/14"),
    spring_start == mdy("1/5/14") ~mdy("1/2/14"),
    spring_start == mdy("1/4/14") ~mdy("1/2/14"),
    spring_start == mdy("1/3/14")~mdy("1/2/14"),
    spring_start == mdy("1/2/14") ~mdy("1/2/14"),
    spring_start > mdy("1/7/14") ~ spring_start - days(7)
  )) 


## looping through all semesters to expand the grid
spring_semester <- -1
for (i in 0:5) {
  spring_semester <- spring_semester + 2
  name <- paste0("semester_", spring_semester)
  calendar <- academic_calendars %>% 
    mutate(spring_start = spring_start + years(i), spring_end = spring_end + years(i)) %>% 
    group_by(university) %>% 
    do(data.frame(semester_number = spring_semester, date = seq(.$spring_start, .$spring_end , by= "day")))
  assign(name, calendar)
}


## looping through all semesters to expand the grid.
winter_semester <- 0
for (i in 0:5) {
  winter_semester <- winter_semester + 2
  name <- paste0("semester_", winter_semester)
  calendar <- academic_calendars %>% 
    mutate(fall_start = fall_start + years(i), fall_end = fall_end + years(i)) %>% 
    group_by(university) %>% 
    do(data.frame(semester_number = winter_semester, date = seq(.$fall_start, .$fall_end , by= "day")))
  assign(name, calendar)
}

## appending all semesters
panel <- bind_rows(semester_1, semester_2,
                   semester_3, semester_4,
                   semester_5, semester_6,
                   semester_7, semester_8,
                   semester_9, semester_10,
                   semester_11, semester_12)

## removing to save data
rm(semester_1, semester_2,
   semester_3, semester_4,
   semester_5, semester_6,
   semester_7, semester_8,
   semester_9, semester_10,
   semester_11, semester_12)

## filtering for only the universities that I have data for at this time.
panel <- panel %>% 
  filter(university %in% universities)






# Merging the crime count data with the panel -----------------------------

## Next Step: Now I need to merge the two datas together - the collapsed data and the large panel.
## joining together the dates and the counts to make a long panel
daily_panel <- panel %>% 
  left_join(collapsed_data_daily, by = c("university" = "university", "date" = "date_reported")) %>% 
  mutate(year = year(date), month = month(date), day = day(date))





# Changing NAs to 0s where applicable + missing data ----------------------

## now I need to pull in data that keeps track of which years I have missing
missing_years <- read_xlsx("data/campus_daily_crime_log/crime_log_list.xlsx",
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
                ~ifelse(year >= 2015 & is.na(.), 0, .)))## changes all missings from 2015 onwards to 0s



daily_panel <- daily_panel %>% 
  mutate(weekday = wday(date, label = T)) ## gets the weekday label


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



# Pulling in school closure data ------------------------------------------

##### Last Step: pulling in the closure data and merging it with this final_panel
closures <- read_xlsx("data/closure_spreadsheet_final_2019.xlsx") %>% 
  janitor::clean_names() %>% 
  select(university, date, deadline, date2, deadline2,date3, deadline3,
         university_enacted_1, university_enacted_2, university_enacted_3, reason1, reason2,reason3) %>% 
  rename("closure_1" = date,
         "closure_1_end" = deadline,
         "closure_2" = date2,
         "closure_2_end" = deadline2,
         "closure_3" = date3,
         "closure_3_end" = deadline3)



daily_panel <- daily_panel %>% 
  left_join(closures, by = c("university" = "university")) %>% 
  mutate(treatment = case_when(
    (!is.na(closure_1) & !is.na(closure_1_end)) & (date >= closure_1 & date < closure_1_end) ~ 1,
    (!is.na(closure_2) & !is.na(closure_2_end)) & (date >= closure_2 & date < closure_2_end) ~ 1,
    (!is.na(closure_3) & !is.na(closure_3_end)) & (date >= closure_3 & date < closure_3_end) ~ 1,
    TRUE ~as.double(0)
  )) %>% 
  mutate(university_enacted = case_when(
    university_enacted_1 == 1 & treatment == 1 ~ 1,
    university_enacted_2 == 1 & treatment == 1 ~ 1,
    university_enacted_3 == 1 & treatment == 1 ~1,
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

weekly_panel_weekends <- daily_panel %>% 
  group_by(university, week = cut(date, "week")) %>% 
  filter(weekday == "Fri" | weekday == "Sat" | weekday == "Sun") %>% 
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

weekly_panel_weekdays <- daily_panel %>% 
  group_by(university, week = cut(date, "week")) %>% 
  filter(!(weekday == "Fri" | weekday == "Sat" | weekday == "Sun")) %>% 
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

## rejoining the closure data and since i took the sum of treatment, creating two variables: 1 that is a percentage of week treated and one that is an indicator
weekly_panel <- weekly_panel %>% 
  left_join(closures, by = c("university" = "university")) %>% 
  mutate(treatment_percent = treatment/7) %>% 
  mutate(treatment = ifelse(treatment > 0, 1, 0)) %>% 
  mutate(year = year(week))

weekly_panel_weekends <- weekly_panel_weekends %>% 
  left_join(closures, by = c("university" = "university")) %>% 
  mutate(treatment_percent = treatment/7) %>% 
  mutate(treatment = ifelse(treatment > 0, 1, 0)) %>% 
  mutate(year = year(week))

weekly_panel_weekdays <- weekly_panel_weekdays %>% 
  left_join(closures, by = c("university" = "university")) %>% 
  mutate(treatment_percent = treatment/7) %>% 
  mutate(treatment = ifelse(treatment > 0, 1, 0)) %>% 
  mutate(year = year(week))


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
            treatment = mean(treatment),
            rape = sum(rape)) %>%
  ungroup() %>% arrange(university, year) 




# Adding in IPEDS ---------------------------------------------------------

### adding in IPEDS data
ipeds <- read_csv("created_data/ipeds/ipeds_final.csv") %>% 
  filter(year > 2013)

daily_panel <-  daily_panel %>% 
  left_join(ipeds, by = c("university" = "university", 'year' = 'year')) %>% 
  mutate(across(c(sexual_assault, alcohol_offense,
                  theft, robbery_burglary, drug_offense, rape), list(ihs = ifc::ihs_transform),
                .names = "{.fn}_{.col}")) %>% 
  mutate(across(c(sexual_assault, alcohol_offense,
                  theft, robbery_burglary, drug_offense, rape), ~./total_enrollment * 25000,
                .names = '{.col}_per25')) %>% 
  filter(university %in% ifc::moratorium_schools()) %>% 
  group_by(university, year, semester_number) %>% 
  mutate(university_by_year_by_semester_number = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, year) %>% 
  mutate(university_by_year = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, year, month) %>% 
  mutate(university_by_month_by_year = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, semester_number) %>% 
  mutate(university_by_semester_number = cur_group_id()) %>% 
  ungroup() %>% 
  rename(day_of_week = weekday) %>% 
  filter(year > 2013) 

yearly_panel <- yearly_panel %>% 
  left_join(ipeds, by= c("university" = "university", 'year' = "year"))

weekly_panel <- weekly_panel %>% 
    left_join(ipeds, by = c("university" = "university", 'year' = 'year')) %>% 
    filter(university %in% ifc::moratorium_schools()) %>% 
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
    group_by(university, year) %>% 
    mutate(university_by_year = cur_group_id()) %>% 
    ungroup() %>% 
    group_by(university, year, month) %>% 
    mutate(university_by_month_by_year = cur_group_id()) %>% 
    ungroup() %>% 
    filter(year > 2013)


weekly_panel_weekends <- weekly_panel_weekends %>% 
  left_join(ipeds, by = c("university" = "university", 'year' = 'year')) %>% 
  filter(university %in% ifc::moratorium_schools()) %>% 
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
  group_by(university, year) %>% 
  mutate(university_by_year = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, year, month) %>% 
  mutate(university_by_month_by_year = cur_group_id()) %>% 
  ungroup() %>% 
  filter(year > 2013)

weekly_panel_weekdays <- weekly_panel_weekdays %>% 
  left_join(ipeds, by = c("university" = "university", 'year' = 'year')) %>% 
  filter(university %in% ifc::moratorium_schools()) %>% 
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
  group_by(university, year) %>% 
  mutate(university_by_year = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, year, month) %>% 
  mutate(university_by_month_by_year = cur_group_id()) %>% 
  ungroup() %>% 
  filter(year > 2013)

# cleaning ----------------------------------------------------------------

## changing reasons to only fit behavior, death, sexual assault, unknown.

daily_panel <- daily_panel %>% 
  mutate(reason1 = ifelse(reason1 == "bad behavior", "behavior", reason1),
         reason1 = ifelse(reason1 == "conduct violation", "behavior", reason1 ),
         reason1 = ifelse(reason1 == "not following rules", "behavior", reason1),
         reason1 = ifelse(reason1 == "racist", "racist activity", reason1),
         reason1 = ifelse(reason1 == "trends", "national trends", reason1), 
         reason1 = ifelse(reason1 == "other", "unknown", reason1),
         reason1 = ifelse(reason1 == "alcohol", "behavior", reason1),
         reason1 = ifelse(reason1 == "hazing", "behavior", reason1),
         reason1 = ifelse(reason1 == "national trends", "unknown", reason1),
         reason1 = ifelse(reason1 == "racist activity", "behavior", reason1)) %>% 
  mutate(reason2 = ifelse(reason2 == "bad behavior", "behavior", reason2),
         reason2 = ifelse(reason2 == "conduct violation", "behavior", reason2 ),
         reason2 = ifelse(reason2 == "not following rules", "behavior", reason2),
         reason2 = ifelse(reason2 == "racist", "racist activity", reason2),
         reason2 = ifelse(reason2 == "trends", "national trends", reason2), 
         reason2 = ifelse(reason2 == "other", "unknown", reason2),
         reason2 = ifelse(reason2 == "alcohol", "behavior", reason2),
         reason2 = ifelse(reason2 == "hazing", "behavior", reason2),
         reason2 = ifelse(reason2 == "national trends", "unknown", reason2),
         reason2 = ifelse(reason2 == "racist activity", "behavior", reason2))

weekly_panel <- weekly_panel %>% 
  mutate(reason1 = ifelse(reason1 == "bad behavior", "behavior", reason1),
         reason1 = ifelse(reason1 == "conduct violation", "behavior", reason1 ),
         reason1 = ifelse(reason1 == "not following rules", "behavior", reason1),
         reason1 = ifelse(reason1 == "racist", "racist activity", reason1),
         reason1 = ifelse(reason1 == "trends", "national trends", reason1), 
         reason1 = ifelse(reason1 == "other", "unknown", reason1),
         reason1 = ifelse(reason1 == "alcohol", "behavior", reason1),
         reason1 = ifelse(reason1 == "hazing", "behavior", reason1),
         reason1 = ifelse(reason1 == "national trends", "unknown", reason1),
         reason1 = ifelse(reason1 == "racist activity", "behavior", reason1)) %>% 
  mutate(reason2 = ifelse(reason2 == "bad behavior", "behavior", reason2),
         reason2 = ifelse(reason2 == "conduct violation", "behavior", reason2 ),
         reason2 = ifelse(reason2 == "not following rules", "behavior", reason2),
         reason2 = ifelse(reason2 == "racist", "racist activity", reason2),
         reason2 = ifelse(reason2 == "trends", "national trends", reason2), 
         reason2 = ifelse(reason2 == "other", "unknown", reason2),
         reason2 = ifelse(reason2 == "alcohol", "behavior", reason2),
         reason2 = ifelse(reason2 == "hazing", "behavior", reason2),
         reason2 = ifelse(reason2 == "national trends", "unknown", reason2),
         reason2 = ifelse(reason2 == "racist activity", "behavior", reason2))

weekly_panel_weekends <- weekly_panel_weekends %>% 
  mutate(reason1 = ifelse(reason1 == "bad behavior", "behavior", reason1),
         reason1 = ifelse(reason1 == "conduct violation", "behavior", reason1 ),
         reason1 = ifelse(reason1 == "not following rules", "behavior", reason1),
         reason1 = ifelse(reason1 == "racist", "racist activity", reason1),
         reason1 = ifelse(reason1 == "trends", "national trends", reason1), 
         reason1 = ifelse(reason1 == "other", "unknown", reason1),
         reason1 = ifelse(reason1 == "alcohol", "behavior", reason1),
         reason1 = ifelse(reason1 == "hazing", "behavior", reason1),
         reason1 = ifelse(reason1 == "national trends", "unknown", reason1),
         reason1 = ifelse(reason1 == "racist activity", "behavior", reason1)) %>% 
  mutate(reason2 = ifelse(reason2 == "bad behavior", "behavior", reason2),
         reason2 = ifelse(reason2 == "conduct violation", "behavior", reason2 ),
         reason2 = ifelse(reason2 == "not following rules", "behavior", reason2),
         reason2 = ifelse(reason2 == "racist", "racist activity", reason2),
         reason2 = ifelse(reason2 == "trends", "national trends", reason2), 
         reason2 = ifelse(reason2 == "other", "unknown", reason2),
         reason2 = ifelse(reason2 == "alcohol", "behavior", reason2),
         reason2 = ifelse(reason2 == "hazing", "behavior", reason2),
         reason2 = ifelse(reason2 == "national trends", "unknown", reason2),
         reason2 = ifelse(reason2 == "racist activity", "behavior", reason2))

weekly_panel_weekdays <- weekly_panel_weekdays %>% 
  mutate(reason1 = ifelse(reason1 == "bad behavior", "behavior", reason1),
         reason1 = ifelse(reason1 == "conduct violation", "behavior", reason1 ),
         reason1 = ifelse(reason1 == "not following rules", "behavior", reason1),
         reason1 = ifelse(reason1 == "racist", "racist activity", reason1),
         reason1 = ifelse(reason1 == "trends", "national trends", reason1), 
         reason1 = ifelse(reason1 == "other", "unknown", reason1),
         reason1 = ifelse(reason1 == "alcohol", "behavior", reason1),
         reason1 = ifelse(reason1 == "hazing", "behavior", reason1),
         reason1 = ifelse(reason1 == "national trends", "unknown", reason1),
         reason1 = ifelse(reason1 == "racist activity", "behavior", reason1)) %>% 
  mutate(reason2 = ifelse(reason2 == "bad behavior", "behavior", reason2),
         reason2 = ifelse(reason2 == "conduct violation", "behavior", reason2 ),
         reason2 = ifelse(reason2 == "not following rules", "behavior", reason2),
         reason2 = ifelse(reason2 == "racist", "racist activity", reason2),
         reason2 = ifelse(reason2 == "trends", "national trends", reason2), 
         reason2 = ifelse(reason2 == "other", "unknown", reason2),
         reason2 = ifelse(reason2 == "alcohol", "behavior", reason2),
         reason2 = ifelse(reason2 == "hazing", "behavior", reason2),
         reason2 = ifelse(reason2 == "national trends", "unknown", reason2),
         reason2 = ifelse(reason2 == "racist activity", "behavior", reason2))

# creating week before and week after -------------------------------------

daily_panel <- daily_panel %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(week_before = lead(treatment, 7)) %>% 
  mutate(week_before = ifelse(treatment ==1 & week_before ==1, 0, week_before)) %>% 
  mutate(week_before_2 = lead(week_before,7)) %>% 
  mutate(week_before_2 = ifelse(treatment == 1 & week_before_2 == 1, 0 , week_before_2)) %>% 
  mutate(week_after = lag(treatment, 7)) %>% 
  mutate(week_after = ifelse(treatment == 1 & week_after == 1, 0 , week_after)) %>% 
  mutate(week_after_2 = lag(week_after, 7)) %>% 
  mutate(week_after_2 = ifelse(treatment == 1 & week_after_2 == 1, 0 , week_after_2)) %>% 
  mutate(across(starts_with("week_"), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(lead_1 = lead(treatment, 7)) %>% 
  mutate(lead_2 = lead(lead_1, 7)) %>% 
  mutate(lag_1 = lag(treatment, 7)) %>% 
  mutate(lag_2 = lag(lag_1, 7)) %>% 
  mutate(across(matches("^lead|^lag"), ~ifelse(is.na(.), 0, .))) %>% 
  relocate(lead_2, lead_1, treatment, lag_1, lag_2, university) %>%  
  # feols(c(sexual_assault_per25, alcohol_offense_per25, drug_offense_per25) ~ week_before_2 + week_before + treatment+ week_after + week_after_2 | day_of_week + university + semester_number,
  #       cluster = ~university, data = .) %>% 
  # filter(day_of_week == "Sat" | day_of_week == "Sun" | day_of_week == "Fri") %>% 
  ungroup() %>% 
  mutate(ifc_enacted = ifelse(university_enacted == 0 & treatment == 1, 1, 0))



# Writing to csv ----------------------------------------------------------

## Note that all of these are only the academic calendars. These are going to be my final
## daily panel
write_csv(daily_panel, file = "created_data/xmaster_data/daily_panel.csv")
haven::write_dta(daily_panel %>% 
                   select(-starts_with("fulltime_"), - starts_with("frac_"), -university_by_year_by_semester_number), path = 'created_data/xmaster_data/daily_panel.dta')
## weekly panel
write_csv(weekly_panel, file = "created_data/xmaster_data/weekly_panel.csv")
write_csv(weekly_panel_weekends, file = "created_data/xmaster_data/weekly_panel_weekends.csv")
write_csv(weekly_panel_weekdays, file = "created_data/xmaster_data/weekly_panel_weekdays.csv")

## yearly panel
write_csv(yearly_panel, file = "created_data/xmaster_data/yearly_panel.csv")




# aggregating to semesters and saving --------------------------------------

treatment_daily_crime <- daily_panel %>% 
  group_by(semester_number, university, year) %>% 
  summarize(treatment = mean(treatment))
semester_level <- daily_panel %>% 
  group_by(semester_number, university, year, .drop = F) %>% 
  summarize(across(c(sexual_assault, alcohol_offense, drug_offense, robbery_burglary), ~sum(.,na.rm = T)))

semester_level <- semester_level %>% 
  left_join(treatment_daily_crime) %>% 
  left_join(ipeds) %>% 
  ungroup() %>% 
  mutate(across(c(sexual_assault, alcohol_offense, drug_offense, robbery_burglary),
                ~(./total_enrollment) * 25000, .names = "{.col}_per25")) %>% 
  mutate(semester_season = ifelse(semester_number %% 2 == 0, 1,2)) %>% 
  group_by(university, semester_season) %>% 
  mutate(university_by_semester_number = cur_group_id()) %>% 
  ungroup()

semester_level <- semester_level %>%  
  group_by(university) %>% 
  arrange(year) %>% 
  mutate(semester_before = lead(treatment)) %>% 
  mutate(semester_after = lag(treatment)) %>% 
  mutate(semester_before = if_else(semester_before >0 & treatment > 0, 0, semester_before)) %>% 
  mutate(semester_after = if_else(semester_after >0  & treatment >0, 0, semester_after)) %>% 
  mutate(semester_before_2 = lead(semester_before)) %>% 
  mutate(semester_after_2 = lag(semester_after)) %>% 
  mutate(semester_before_2 = if_else(semester_before_2 >0 & treatment > 0, 0, semester_before_2)) %>% 
  mutate(semester_after_2 = if_else(semester_after_2 >0  & treatment >0, 0, semester_after_2)) %>% 
  mutate(across(c(semester_before, semester_after,
                  semester_before_2, semester_after_2), ~ifelse(. >0, 1, 0))) %>% 
  mutate(across(c(semester_before, semester_after,
                  semester_before_2, semester_after_2), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(lead_1 = lead(treatment)) %>% 
  mutate(lead_2= lead(lead_1)) %>% 
  mutate(lag_1= lag(treatment)) %>% 
  mutate(lag_2 = lag(lag_1)) %>% 
  mutate(across(c("lead_1", "lead_2",
                  "lag_1",  "lag_2"), ~ifelse(is.na(.), 0, .))) %>% 
  ungroup() %>% 
  group_by(university, semester_number) %>% 
  mutate(university_by_year_by_semester_number = cur_group_id()) %>% 
  ungroup()


# semester weekends only --------------------------------------------------


daily_panel_weekends <- daily_panel %>%
  filter(day_of_week == 'Fri' | day_of_week == "Sat" | day_of_week == "Sun")

treatment_daily_crime_weekends <- daily_panel_weekends %>% 
  group_by(semester_number, university, year) %>% 
  summarize(treatment = mean(treatment))
semester_level_weekends <- daily_panel_weekends %>% 
  group_by(semester_number, university, year, .drop = F) %>% 
  summarize(across(c(sexual_assault, alcohol_offense, drug_offense, robbery_burglary), ~sum(.,na.rm = T)))


semester_level_weekends <- semester_level_weekends %>% 
  left_join(treatment_daily_crime_weekends) %>% 
  left_join(ipeds) %>% 
  mutate(across(c(sexual_assault, alcohol_offense, drug_offense, robbery_burglary),
                ~(./total_enrollment) * 25000, .names = "{.col}_per25")) %>% 
  ungroup() %>% 
  mutate(semester_season = ifelse(semester_number %% 2 == 0, 1,2)) %>% 
  group_by(university, semester_season) %>% 
  mutate(university_by_semester_number = cur_group_id()) %>% 
  ungroup()



## indicators  before and after
semester_level_weekends <- semester_level_weekends %>% 
  group_by(university) %>% 
  arrange(year) %>% 
  mutate(semester_before = lead(treatment)) %>% 
  mutate(semester_after = lag(treatment)) %>% 
  mutate(semester_before = if_else(semester_before >0 & treatment > 0, 0, semester_before)) %>% 
  mutate(semester_after = if_else(semester_after >0  & treatment >0, 0, semester_after)) %>% 
  mutate(semester_before_2 = lead(semester_before)) %>% 
  mutate(semester_after_2 = lag(semester_after)) %>% 
  mutate(semester_before_2 = if_else(semester_before_2 >0 & treatment > 0, 0, semester_before_2)) %>% 
  mutate(semester_after_2 = if_else(semester_after_2 >0  & treatment >0, 0, semester_after_2)) %>% 
  mutate(across(c(semester_before, semester_after,
                  semester_before_2, semester_after_2), ~ifelse(. >0, 1, 0))) %>% 
  mutate(across(c(semester_before, semester_after,
                  semester_before_2, semester_after_2), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(lead_1 = lead(treatment)) %>% 
  mutate(lead_2= lead(lead_1)) %>% 
  mutate(lag_1= lag(treatment)) %>% 
  mutate(lag_2 = lag(lag_1)) %>% 
  mutate(across(c("lead_1", "lead_2",
                  "lag_1",  "lag_2"), ~ifelse(is.na(.), 0, .))) %>% 
  ungroup() %>% 
  group_by(university, semester_number) %>% 
  mutate(university_by_year_by_semester_number = cur_group_id()) %>% 
  ungroup()


# semester weekdays only --------------------------------------------------

daily_panel_weekdays <- daily_panel %>% 
  filter(day_of_week == "Mon" | day_of_week == "Thu" | day_of_week == "Wed" | day_of_week == "Tue" )

## weekdays 
treatment_daily_crime_weekdays <- daily_panel_weekdays %>% 
  group_by(semester_number, university, year) %>% 
  summarize(treatment = mean(treatment))
semester_level_weekdays <- daily_panel_weekdays %>% 
  group_by(semester_number, university, year, .drop = F) %>% 
  summarize(across(c(sexual_assault, alcohol_offense, drug_offense, robbery_burglary), ~sum(.,na.rm = T)))

semester_level_weekdays <- semester_level_weekdays %>% 
  left_join(treatment_daily_crime_weekdays) %>% 
  left_join(ipeds) %>% 
  mutate(across(c(sexual_assault, alcohol_offense, drug_offense, robbery_burglary),
                ~(./total_enrollment) * 25000, .names = "{.col}_per25")) %>% 
  ungroup() %>% 
  mutate(semester_season = ifelse(semester_number %% 2 == 0, 1,2)) %>% 
  group_by(university, semester_season) %>% 
  mutate(university_by_semester_number = cur_group_id()) %>% 
  ungroup()

## indicators before and after moratorium
semester_level_weekdays <- semester_level_weekdays %>% 
  group_by(university) %>% 
  arrange(year) %>% 
  mutate(semester_before = lead(treatment)) %>% 
  mutate(semester_after = lag(treatment)) %>% 
  mutate(semester_before = if_else(semester_before >0 & treatment > 0, 0, semester_before)) %>% 
  mutate(semester_after = if_else(semester_after >0  & treatment >0, 0, semester_after)) %>% 
  mutate(semester_before_2 = lead(semester_before)) %>% 
  mutate(semester_after_2 = lag(semester_after)) %>% 
  mutate(semester_before_2 = if_else(semester_before_2 >0 & treatment > 0, 0, semester_before_2)) %>% 
  mutate(semester_after_2 = if_else(semester_after_2 >0  & treatment >0, 0, semester_after_2)) %>% 
  mutate(across(c(semester_before, semester_after,
                  semester_before_2, semester_after_2), ~ifelse(. >0, 1, 0))) %>% 
  mutate(across(c(semester_before, semester_after,
                  semester_before_2, semester_after_2), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(lead_1 = lead(treatment)) %>% 
  mutate(lead_2= lead(lead_1)) %>% 
  mutate(lag_1= lag(treatment)) %>% 
  mutate(lag_2 = lag(lag_1)) %>% 
  mutate(across(c("lead_1", "lead_2",
                  "lag_1",  "lag_2"), ~ifelse(is.na(.), 0, .))) %>% 
  ungroup() %>% 
  group_by(university, semester_number) %>% 
  mutate(university_by_year_by_semester_number = cur_group_id()) %>% 
  ungroup()


# create quartiles and semester spring/fall -------------------------------

get_quartile_columns <- function(df) {
  ## find the quartiles of the treatment
  quartiles <- df %>% 
    filter(treatment > 0) %>% 
    summarize(qunatile_treatments = quantile(treatment, c(0.25, 0.5, 0.75))) %>% 
    pull()
  ## put in indicators for quartiles and semester season
  df <- df %>% 
    mutate(quartile_1 = ifelse(between(semester_level$treatment,0.001, quartiles[[1]] ) , 1, 0),
           quartile_2 = ifelse(between(treatment, quartiles[[1]], quartiles[[2]]), 1 ,0),
           quartile_3 = ifelse(between(treatment, quartiles[[2]], quartiles[[3]]), 1, 0),
           quartile_4 = ifelse(between(treatment, quartiles[[3]], 1), 1, 0)) %>% 
    mutate(fall_semester = ifelse(semester_number %%2 == 0, 1, 0),
           spring_semester = ifelse(semester_number %% 2 == 1, 1, 0))
  return(df)
}

semester_level <- get_quartile_columns(semester_level)
semester_level_weekdays <- get_quartile_columns(semester_level_weekdays)
semester_level_weekends <- get_quartile_columns(semester_level_weekends)


# writing semester files --------------------------------------------------


write_csv(semester_level, file = "created_data/xmaster_data/semester_level.csv")
write_csv(semester_level_weekends, file = "created_data/xmaster_data/semester_level_weekends.csv")
write_csv(semester_level_weekdays, file ="created_data/xmaster_data/semester_level_weekdays.csv")

write_csv(daily_panel_weekends , fil = "created_data/xmaster_data/daily_panel_weekends.csv")
write_csv(daily_panel_weekdays, file = "created_data/xmaster_data/daily_panel_weekdays.csv")