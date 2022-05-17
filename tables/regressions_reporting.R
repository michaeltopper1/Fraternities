library(tidyverse)
library(lubridate)
library(readxl)
library(fixest)
library(modelsummary)
library(kableExtra)

appended_crime_logs <- read_csv("created_data/xmaster_data/appended_crime_logs.csv") %>% 
  filter(university %in% ifc::moratorium_schools())

## number of schools that have date occurred
universities_date_occurred <- appended_crime_logs %>% 
  mutate(report_lag = date_reported - date_occurred) %>% relocate(report_lag) %>% 
  filter(report_lag >=0 | is.na(report_lag)) %>% 
  filter(!is.na(date_occurred)) %>% 
  distinct(university) %>% pull()

## number of crimes that have date occurred
number_crimes_occurred <- appended_crime_logs %>% 
  mutate(report_lag = date_reported - date_occurred) %>% relocate(report_lag) %>% 
  filter(report_lag >=0 | is.na(report_lag)) %>% 
  filter(!is.na(date_occurred)) %>% 
  nrow

## this data gives me the indicators for the proportion of crimes that had a lag
## deletes any lags that are negative because they don't make sense
## delete out any date_occurred that don't exist?
collapsed_data_daily <- appended_crime_logs %>% 
  mutate(report_lag = date_reported - date_occurred) %>% relocate(report_lag) %>% 
  filter(report_lag >=0 | is.na(report_lag)) %>% 
  filter(!is.na(date_occurred)) %>% 
  mutate(report_lag_alc_1 = ifelse(report_lag > 1 & alcohol_offense == 1, 1, 0)) %>% 
  mutate(report_lag_sex_1 = ifelse(report_lag > 1 & sexual_assault == 1, 1, 0)) %>% 
  mutate(report_lag_drug_1 = ifelse(report_lag > 1 & drug_offense == 1, 1, 0)) %>% 
  mutate(report_lag_rob_1 = ifelse(report_lag > 1 & robbery_burglary == 1, 1, 0)) %>% 
  mutate(report_lag_theft_1 = ifelse(report_lag > 1 & theft == 1, 1, 0)) %>% 
  mutate(report_lag_alc_3 = ifelse(report_lag > 3 & alcohol_offense == 1, 1, 0)) %>% 
  mutate(report_lag_sex_3 = ifelse(report_lag > 3 & sexual_assault == 1, 1, 0)) %>% 
  mutate(report_lag_drug_3 = ifelse(report_lag > 3 & drug_offense == 1, 1, 0)) %>% 
  mutate(report_lag_rob_3 = ifelse(report_lag > 3 & robbery_burglary == 1, 1, 0)) %>% 
  mutate(report_lag_theft_3 = ifelse(report_lag > 3 & theft == 1, 1, 0)) %>% 
  mutate(report_lag_alc_7 = ifelse(report_lag > 7 & alcohol_offense == 1, 1, 0)) %>% 
  mutate(report_lag_sex_7 = ifelse(report_lag >  7& sexual_assault == 1, 1, 0)) %>% 
  mutate(report_lag_drug_7 = ifelse(report_lag > 7 & drug_offense == 1, 1, 0)) %>% 
  mutate(report_lag_rob_7 = ifelse(report_lag > 7 & robbery_burglary == 1, 1, 0)) %>% 
  mutate(report_lag_theft_7 = ifelse(report_lag > 7 & theft == 1, 1, 0)) %>% 
  mutate(report_lag_alc_14 = ifelse(report_lag > 14 & alcohol_offense == 1, 1, 0)) %>% 
  mutate(report_lag_sex_14 = ifelse(report_lag > 14 & sexual_assault == 1, 1, 0)) %>% 
  mutate(report_lag_drug_14 = ifelse(report_lag > 14 & drug_offense == 1, 1, 0)) %>% 
  mutate(report_lag_rob_14 = ifelse(report_lag > 14 & robbery_burglary == 1, 1, 0)) %>% 
  mutate(report_lag_theft_14 = ifelse(report_lag > 14 & theft == 1, 1, 0)) %>% 
  relocate(report_lag_alc, alcohol_offense, report_lag_sex, sexual_assault) %>% 
  select(date_reported, university, sexual_assault, alcohol_offense, drug_offense, theft, robbery_burglary, alcohol_offense_strict,
         noise_offense, rape, starts_with("report_lag_")) %>% 
  group_by(university, date_reported) %>% 
  summarize(across(everything(), ~sum(.,na.rm = T))) 




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
    spring_start <= as.Date("1/7/14") ~spring_start,
    spring_start > as.Date("1/7/14") ~spring_start - days(7)
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

## filtering to only universities that i have date occurred information for
panel <- panel %>% 
  filter(university %in% universities_date_occurred)


date_occurred_panel <- panel %>% 
  left_join(collapsed_data_daily, by = c("university" = "university", "date" = "date_reported")) %>% 
  mutate(year = year(date), month = month(date), day = day(date))




## now I need to pull in data that keeps track of which years I have missing
missing_years <- read_xlsx("data/campus_daily_crime_log/crime_log_list.xlsx",
                           sheet = "missing_years") %>% janitor::clean_names()


## creates a list of schools missing year 2014
not_missing_2014 <- missing_years %>% 
  filter(missing_2014 == 0) %>% 
  pull(university)


## replaces all days with NAs with 0s if i had data for that year
## i did this because i can assume there were 0 reports on a date that there was no crime in years i have
date_occurred_panel <- date_occurred_panel %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape",
                  "report_lag_alc_3", "report_lag_sex_3", "report_lag_drug_3", "report_lag_theft_3", "report_lag_rob_3",
                  "report_lag_alc_1", "report_lag_sex_1", "report_lag_drug_1", "report_lag_theft_1", "report_lag_rob_1",
                  "report_lag_alc_7", "report_lag_sex_7", "report_lag_drug_7", "report_lag_theft_7", "report_lag_rob_7",
                  "report_lag_alc_14", "report_lag_sex_14", "report_lag_drug_14", "report_lag_theft_14", "report_lag_rob_14"), 
                ~ifelse(year== 2014 & is.na(.) & university %in% not_missing_2014,
                        0, .))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape",
                  "report_lag_alc_3", "report_lag_sex_3", "report_lag_drug_3", "report_lag_theft_3", "report_lag_rob_3",
                  "report_lag_alc_1", "report_lag_sex_1", "report_lag_drug_1", "report_lag_theft_1", "report_lag_rob_1",
                  "report_lag_alc_7", "report_lag_sex_7", "report_lag_drug_7", "report_lag_theft_7", "report_lag_rob_7",
                  "report_lag_alc_14", "report_lag_sex_14", "report_lag_drug_14", "report_lag_theft_14", "report_lag_rob_14"), 
                ~ifelse(year >= 2015 & is.na(.), 0, .)))## changes all missings from 2015 onwards to 0s



date_occurred_panel <- date_occurred_panel %>% 
  mutate(weekday = wday(date, label = T)) ## gets the weekday label


## Ferrum College is missing data. only have september 2015 - 2019
## Delaware state only has data from 2017 - onwards
## Texas Austin is missing jan/feb 2016
## NC state is missing data from August 2014 and before
date_occurred_panel <- date_occurred_panel %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape",
                  "report_lag_alc_3", "report_lag_sex_3", "report_lag_drug_3", "report_lag_theft_3", "report_lag_rob_3",
                  "report_lag_alc_1", "report_lag_sex_1", "report_lag_drug_1", "report_lag_theft_1", "report_lag_rob_1",
                  "report_lag_alc_7", "report_lag_sex_7", "report_lag_drug_7", "report_lag_theft_7", "report_lag_rob_7",
                  "report_lag_alc_14", "report_lag_sex_14", "report_lag_drug_14", "report_lag_theft_14", "report_lag_rob_14"), ~ifelse(
    year == 2015 & month < 9 & university == "Ferrum College",
    NA, .
  ))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape",
                  "report_lag_alc_3", "report_lag_sex_3", "report_lag_drug_3", "report_lag_theft_3", "report_lag_rob_3",
                  "report_lag_alc_1", "report_lag_sex_1", "report_lag_drug_1", "report_lag_theft_1", "report_lag_rob_1",
                  "report_lag_alc_7", "report_lag_sex_7", "report_lag_drug_7", "report_lag_theft_7", "report_lag_rob_7",
                  "report_lag_alc_14", "report_lag_sex_14", "report_lag_drug_14", "report_lag_theft_14", "report_lag_rob_14"), ~ifelse(
    year <= 2016 & university == "Delaware State University",
    NA, .
  ))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape",
                  "report_lag_alc_3", "report_lag_sex_3", "report_lag_drug_3", "report_lag_theft_3", "report_lag_rob_3",
                  "report_lag_alc_1", "report_lag_sex_1", "report_lag_drug_1", "report_lag_theft_1", "report_lag_rob_1",
                  "report_lag_alc_7", "report_lag_sex_7", "report_lag_drug_7", "report_lag_theft_7", "report_lag_rob_7",
                  "report_lag_alc_14", "report_lag_sex_14", "report_lag_drug_14", "report_lag_theft_14", "report_lag_rob_14"), ~ifelse(
    year== 2016 & (month == 1 | month == 2) & university == "The University of Texas at Austin",
    NA, .
  ))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape",
                  "report_lag_alc_3", "report_lag_sex_3", "report_lag_drug_3", "report_lag_theft_3", "report_lag_rob_3",
                  "report_lag_alc_1", "report_lag_sex_1", "report_lag_drug_1", "report_lag_theft_1", "report_lag_rob_1",
                  "report_lag_alc_7", "report_lag_sex_7", "report_lag_drug_7", "report_lag_theft_7", "report_lag_rob_7",
                  "report_lag_alc_14", "report_lag_sex_14", "report_lag_drug_14", "report_lag_theft_14", "report_lag_rob_14"), ~ifelse(
    year== 2014 & (month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6 | month == 7| month == 8) & university == "North Carolina State University at Raleigh",
    NA, .
  )))



# Pulling in school closure data ------------------------------------------

##### Last Step: pulling in the closure data and merging it with this final_panel
closures <- read_xlsx("data/closure_spreadsheet_final_2019.xlsx") %>% 
  janitor::clean_names() %>% 
  select(university, date, deadline, date2, deadline2,date3, deadline3,
         university_enacted_1, university_enacted_2, university_enacted_3, reason1, reason2, reason3) %>% 
  rename("closure_1" = date,
         "closure_1_end" = deadline,
         "closure_2" = date2,
         "closure_2_end" = deadline2,
         "closure_3" = date3,
         "closure_3_end" = deadline3)



date_occurred_panel <- date_occurred_panel %>% 
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
    university_enacted_3 == 1 & treatment == 1 ~ 1,
    TRUE ~as.double(0)
  ))


date_occurred_panel <- date_occurred_panel %>% 
  mutate(day_of_week = lubridate::wday(date, label = T)) %>% 
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

library(timeDate)
veterans <- as.character(timeDate::holiday(2014:2019, "USVeteransDay")) 
thanksgiving <- as.character(timeDate::holiday(2014:2019, "USThanksgivingDay"))  
labor <- as.character(timeDate::holiday(2014:2019, "USLaborDay"))
halloween <- c("2014-10-31", "2015-10-31", "2016-10-31", "2017-10-31", "2018-10-31", "2019-10-31")
mlk <- as.character(timeDate::holiday(2014:2019, "USMLKingsBirthday"))
detach("package:timeDate", unload = TRUE)

date_occurred_panel <- date_occurred_panel %>% 
  mutate(labor = ifelse(date %in% lubridate::as_date(labor), 1, 0)) %>% 
  mutate(thanksgiving = ifelse(date %in% lubridate::as_date(thanksgiving), 1, 0)) %>% 
  mutate(halloween = ifelse(date %in% lubridate::as_date(halloween), 1, 0)) %>% 
  mutate(mlk = ifelse(date %in% lubridate::as_date(mlk), 1, 0)) %>% 
  mutate(veterans = ifelse(date %in% lubridate::as_date(veterans), 1, 0)) %>% 
  mutate(holiday = ifelse(labor == 1 | thanksgiving == 1 | halloween == 1 | mlk == 1 | veterans == 1, 1, 0))




# creating other weekend/weekday panels -----------------------------------

date_occurred_panel_weekends <- date_occurred_panel %>% 
  filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun")

date_occurred_panel_weekdays <- date_occurred_panel %>% 
  filter(day_of_week == "Mon" | day_of_week == "Tue" | day_of_week == "Wed" | day_of_week == "Thu")

lag_datas <- list(date_occurred_panel, date_occurred_panel_weekends, date_occurred_panel_weekdays)

## now finally creating the columns for "proportion of crimes with reporting lag"

lag_datas <- map(lag_datas, ~.x %>% 
      mutate(proportion_alc_lag_1 = ifelse(alcohol_offense == 0, 0, report_lag_alc_1/alcohol_offense),
             proportion_sex_lag_1 = ifelse(sexual_assault == 0, 0, report_lag_sex_1/sexual_assault),
             proportion_drug_lag_1 = ifelse(drug_offense == 0, 0, report_lag_drug_1/drug_offense),
             proportion_theft_lag_1 = ifelse(theft == 0, 0, report_lag_theft_1/theft),
             proportion_rob_lab_1 = ifelse(robbery_burglary == 0, 0, report_lag_rob_1/robbery_burglary),
             proportion_alc_lag_3 = ifelse(alcohol_offense == 0, 0, report_lag_alc_3/alcohol_offense),
             proportion_sex_lag_3 = ifelse(sexual_assault == 0, 0, report_lag_sex_3/sexual_assault),
             proportion_drug_lag_3 = ifelse(drug_offense == 0, 0, report_lag_drug_3/drug_offense),
             proportion_theft_lag_3 = ifelse(theft == 0, 0, report_lag_theft_3/theft),
             proportion_rob_lab_3 = ifelse(robbery_burglary == 0, 0, report_lag_rob_3/robbery_burglary),
             proportion_alc_lag_7 = ifelse(alcohol_offense == 0, 0, report_lag_alc_7/alcohol_offense),
             proportion_sex_lag_7 = ifelse(sexual_assault == 0, 0, report_lag_sex_7/sexual_assault),
             proportion_drug_lag_7 = ifelse(drug_offense == 0, 0, report_lag_drug_7/drug_offense),
             proportion_theft_lag_7 = ifelse(theft == 0, 0, report_lag_theft_7/theft),
             proportion_rob_lab_7 = ifelse(robbery_burglary == 0, 0, report_lag_rob_7/robbery_burglary),
             proportion_alc_lag_14 = ifelse(alcohol_offense == 0, 0, report_lag_alc_14/alcohol_offense),
             proportion_sex_lag_14 = ifelse(sexual_assault == 0, 0, report_lag_sex_14/sexual_assault),
             proportion_drug_lag_14 = ifelse(drug_offense == 0, 0, report_lag_drug_14/drug_offense),
             proportion_theft_lag_14 = ifelse(theft == 0, 0, report_lag_theft_14/theft),
             proportion_rob_lab_14 = ifelse(robbery_burglary == 0, 0, report_lag_rob_14/robbery_burglary)) )

date_occurred_panel <- lag_datas[[1]]
date_occurred_panel_weekends <- lag_datas[[2]]
date_occurred_panel_weekdays <- lag_datas[[3]]



# mergining in game days --------------------------------------------------

football_schools <- read_csv("created_data/xmaster_data/football_final.csv")

date_occurred_panel <- date_occurred_panel %>% 
  left_join(football_schools, by = c("university" = "school", "date" = "game_date"))

lag_regression <- date_occurred_panel %>% 
  feols(c(proportion_sex_lag_7, proportion_alc_lag_7) ~ treatment |
          date + university, cluster = ~university, data = .) 


fe <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")
explanatory_vars <- c("treatment")

lag_variables_alc <- c("proportion_alc_lag_1", "proportion_alc_lag_3","proportion_alc_lag_7","proportion_alc_lag_14")
lag_variables_sex <- c("proportion_sex_lag_1","proportion_sex_lag_3", "proportion_sex_lag_7","proportion_sex_lag_14")
lag_alc <- map(lag_variables_alc, ~ifc::reghdfe(date_occurred_panel, ., explanatory_vars = explanatory_vars, fe, "university"))
lag_sex <- map(lag_variables_sex, ~ifc::reghdfe(date_occurred_panel, ., explanatory_vars = explanatory_vars, fe, "university"))




# final table -------------------------------------------------------------



reporting_table <- ifc::main_table(lag_alc, last_panel = lag_sex) %>% 
  slice(1:6) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(date_occurred_panel$proportion_alc_lag_1, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(date_occurred_panel$proportion_alc_lag_3, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(date_occurred_panel$proportion_alc_lag_7, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(date_occurred_panel$proportion_alc_lag_14, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(date_occurred_panel$proportion_sex_lag_1, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(date_occurred_panel$proportion_sex_lag_3, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(date_occurred_panel$proportion_sex_lag_7, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(date_occurred_panel$proportion_alc_lag_14, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T,
      col.names = c(" ","(1)","(2)", "(3)", "(4)"),
      caption = "\\label{reporting_table}Effect of Moratoriums on Changes in Reporting",
      align = "lcccc") %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11) %>% 
  pack_rows("Panel A: Proportion of Alcohol Offenses Reported with Lag", 1, 4, italic = T, bold = F) %>%
  pack_rows("Panel B: Proportion of Sexual Assaults Reported with Lag", 5, 8, italic = T, bold = F,  latex_gap_space = "0.5cm") %>%
  add_header_above(c(" ", "More than 1-Day Lag","More than 3-Day Lag", "More than 7-Day Lag", "More than 14-day Lag"), line = F) %>% 
  add_header_above(c(" " = 1, "Reporting Lag" = 4)) %>% 
  row_spec(8,hline_after = T) %>% 
  footnote(list("Standard errors are clustered by university.  Panels A and B are OLS regressions of proportions of alcohol offenses and sexual assaults reported with a reporting lag. A reporting lag is defined as an offense that was reported more than one (Column 1), three (Column 2), seven (Column 3), or 14 (Column 4) days after it occurred. 32 of the 37 universities have information on date occurred. Specification is the preferred specification which includes day of week, holiday, football game-day, semester, and university-by-academic-year fixed effects. See Table 4 column (2) for more details on the preferred specification.",
                "* p < 0.1, ** p < 0.05, *** p < 0.01"),
           threeparttable = T)





