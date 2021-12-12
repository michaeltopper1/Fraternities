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
  left_join(collapsed_data_daily, by = c("university" = "university", "date" = "date_preferred")) %>% ## changed here! 
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

daily_panel <- daily_panel %>% 
  mutate(academic_year = case_when(
    semester_number == 1 ~ 1, ## 2014 spring
    semester_number == 2 | semester_number == 3 ~ 2, ## 2015 
    semester_number == 4 | semester_number == 5 ~ 3,
    semester_number == 6 | semester_number == 7 ~ 4,
    semester_number == 8 | semester_number == 9 ~ 5,
    semester_number == 10 | semester_number == 11 ~ 6,
    semester_number == 12 ~7 ## 2019 fall
  )) %>% 
  group_by(university, academic_year) %>% 
  mutate(university_by_academic_year = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(day_of_week, month) %>% 
  mutate(day_of_week_by_month = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(day_of_week, month, academic_year) %>% 
  mutate(day_of_week_by_month_by_academic_year = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, month, day_of_week) %>% 
  mutate(university_by_day_of_week_by_month = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, month, day_of_week, academic_year) %>% 
  mutate(university_by_academic_year_by_month_by_day_of_week = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(spring_semester = ifelse(semester_number %% 2 == 0,1, 0)) %>% 
  group_by(day_of_week, spring_semester) %>% 
  mutate(day_of_week_by_spring_semester = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(day_of_week, semester_number) %>% 
  mutate(day_of_week_by_semester_number = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, month, academic_year) %>% 
  mutate(university_by_month_by_academic_year = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(academic_year, spring_semester) %>% 
  mutate(semester_by_academic_year = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, spring_semester, academic_year) %>% 
  mutate(university_by_academic_year_by_semester = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(day_of_week, semester_by_academic_year) %>% 
  mutate(day_of_week_by_semester_by_academic_year = cur_group_id()) %>% 
  ungroup() %>% 
  mutate(week = lubridate::week(date)) %>% 
  group_by(university, month) %>% 
  mutate(university_by_month = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, week) %>% 
  mutate(university_by_week = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, academic_year, week) %>% 
  mutate(university_by_academic_year_by_week = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, year) %>% 
  mutate(university_by_calendar_year = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(university, spring_semester) %>% 
  mutate(university_by_spring_semester = cur_group_id()) %>% 
  ungroup()


# adding in holiday controls ----------------------------------------------
library(timeDate)
veterans <- as.character(timeDate::holiday(2014:2019, "USVeteransDay")) 
thanksgiving <- as.character(timeDate::holiday(2014:2019, "USThanksgivingDay"))  
labor <- as.character(timeDate::holiday(2014:2019, "USLaborDay"))
halloween <- c("2014-10-31", "2015-10-31", "2016-10-31", "2017-10-31", "2018-10-31", "2019-10-31")
mlk <- as.character(timeDate::holiday(2014:2019, "USMLKingsBirthday"))
detach("package:timeDate", unload = TRUE)

daily_panel <- daily_panel %>% 
  mutate(labor = ifelse(date %in% lubridate::as_date(labor), 1, 0)) %>% 
  mutate(thanksgiving = ifelse(date %in% lubridate::as_date(thanksgiving), 1, 0)) %>% 
  mutate(halloween = ifelse(date %in% lubridate::as_date(halloween), 1, 0)) %>% 
  mutate(mlk = ifelse(date %in% lubridate::as_date(mlk), 1, 0)) %>% 
  mutate(veterans = ifelse(date %in% lubridate::as_date(veterans), 1, 0)) %>% 
  mutate(holiday = ifelse(labor == 1 | thanksgiving == 1 | halloween == 1 | mlk == 1 | veterans == 1, 1, 0))



# omitting week before for sexual assault only ----------------------------

daily_panel <- daily_panel %>% 
  filter(!(university == "Arkansas State University-Main Campus" & (date >= "2017-02-14"& date <"2017-02-21"))) %>% 
  filter(!(university == "Clemson University" & (date >=  "2014-09-16" & date < "2014-09-23"))) %>% 
  filter(!(university == "East Carolina University" & (date >= "2015-01-21" & date < "2015-01-28"))) %>% 
  filter(!(university == "Emory University" & (date >= "2014-10-27" & date < "2014-11-03"))) %>% 
  filter(!(university == "North Carolina State University at Raleigh" & (date >= "2015-03-13" & date < "2015-03-20"))) %>% 
  filter(!(university == "San Diego State University" & (date >= "2014-11-18" & date < "2014-11-25"))) %>% 
  filter(!(university == "University of California-Berkeley" & (date >= "2016-10-09" & date < "2016-10-16"))) %>% 
  filter(!(university == "University of Michigan-Ann Arbor" & (date >= "2017-11-02" & date < "2017-11-09"))) %>% 
  filter(!(university == "University of Virginia-Main Campus" & (date >= "2014-11-15" & date < "2014-11-22"))) %>% 
  filter(!(university == "California Polytechnic State University-San Luis Obispo" & (date >= "2018-04-10" & date < "2018-04-17"))) 


# adding in indicators for 2 and 1 weeks before and after -----------------

leead <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + lead(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}

laag <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + dplyr::lag(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}

daily_panel <- daily_panel %>% 
  group_by(university) %>%
  arrange(date) %>%
  mutate(day_one_moratorium = case_when(
    closure_1 == date ~ 1,
    closure_2 == date ~ 1,
    closure_3 == date ~ 1,
    university == "Florida International University" & date == "2018-01-04" ~ 1,
    TRUE ~as.double(0))) %>% 
  mutate(final_day_moratorium = case_when(
    closure_1_end == date + lubridate::days(1) ~ 1,
    closure_2_end == date +lubridate::days(1)~ 1,
    closure_3_end == date + lubridate::days(1)~1,
    university == "San Diego State University" & date == "2014-12-16" ~1,
    university == "University of Virginia-Main Campus" & date == "2014-12-18" ~ 1,
    university == "Washington State University" & date == "2016-12-22" ~1,
    university == "Monmouth University" & date == "2018-12-23" ~1,
    TRUE ~ as.double(0)
  )) %>% 
  mutate(week_before = leead(day_one_moratorium, c(1:7)),
         week_after = laag(final_day_moratorium, c(1:7)),
         two_weeks_before = leead(day_one_moratorium, c(8:14)),
         two_weeks_after = laag(final_day_moratorium, c(8:14))) %>% 
  ungroup()


daily_panel_weekdays <- daily_panel %>% 
  filter(day_of_week == "Mon" | day_of_week == "Thu" | day_of_week == "Wed" | day_of_week == "Tue" )
daily_panel_weekends <- daily_panel %>%
  filter(day_of_week == 'Fri' | day_of_week == "Sat" | day_of_week == "Sun")



# Writing to csv ----------------------------------------------------------

## Note that all of these are only the academic calendars. These are going to be my final
## daily panel
write_csv(daily_panel, file = "created_data/xmaster_data/daily_panel.csv")
write_csv(daily_panel_weekends , file = "created_data/xmaster_data/daily_panel_weekends.csv")
write_csv(daily_panel_weekdays, file = "created_data/xmaster_data/daily_panel_weekdays.csv")

# haven::write_dta(daily_panel %>% 
#                    select(-starts_with("fulltime_"), - starts_with("frac_"), -university_by_year_by_semester_number), path = 'created_data/xmaster_data/daily_panel.dta')
## weekly panel
write_csv(weekly_panel, file = "created_data/xmaster_data/weekly_panel.csv")
write_csv(weekly_panel_weekends, file = "created_data/xmaster_data/weekly_panel_weekends.csv")
write_csv(weekly_panel_weekdays, file = "created_data/xmaster_data/weekly_panel_weekdays.csv")

## yearly panel
write_csv(yearly_panel, file = "created_data/xmaster_data/yearly_panel.csv")


