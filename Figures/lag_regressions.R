library(tidyverse)
library(lubridate)
library(readxl)
library(fixest)
library(modelsummary)

appended_crime_logs <- read_csv("Created Data/xMaster_data_2021/appended_crime_logs.csv") %>% 
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
  mutate(report_lag_alc = ifelse(report_lag > 3 & alcohol_offense == 1, 1, 0)) %>% 
  mutate(report_lag_sex = ifelse(report_lag > 3 & sexual_assault == 1, 1, 0)) %>% 
  mutate(report_lag_drug = ifelse(report_lag > 3 & drug_offense == 1, 1, 0)) %>% 
  mutate(report_lag_rob = ifelse(report_lag > 3 & robbery_burglary == 1, 1, 0)) %>% 
  mutate(report_lag_theft = ifelse(report_lag > 3 & theft == 1, 1, 0)) %>% 
  relocate(report_lag_alc, alcohol_offense, report_lag_sex, sexual_assault) %>% 
  select(date_reported, university, sexual_assault, alcohol_offense, drug_offense, theft, robbery_burglary, alcohol_offense_strict,
         noise_offense, rape, starts_with("report_lag_")) %>% 
  group_by(university, date_reported) %>% 
  summarize(across(everything(), ~sum(.,na.rm = T))) 




# Creating Academic Calendar panels by semester ---------------------------

## pulling in the academic calendars
academic_calendars <- read_csv("Data/academic_calendars.csv") %>% janitor::clean_names()

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
missing_years <- read_xlsx("Data/campus_daily_crime_log/crime_log_list.xlsx",
                           sheet = "missing_years") %>% janitor::clean_names()


## creates a list of schools missing year 2014
not_missing_2014 <- missing_years %>% 
  filter(missing_2014 == 0) %>% 
  pull(university)


## replaces all days with NAs with 0s if i had data for that year
## i did this because i can assume there were 0 reports on a date that there was no crime in years i have
date_occurred_panel <- date_occurred_panel %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape",
                  "report_lag_alc", "report_lag_sex", "report_lag_drug", "report_lag_theft", "report_lag_rob"), 
                ~ifelse(year== 2014 & is.na(.) & university %in% not_missing_2014,
                        0, .))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape",
                  "report_lag_alc", "report_lag_sex", "report_lag_drug", "report_lag_theft", "report_lag_rob"), 
                ~ifelse(year >= 2015 & is.na(.), 0, .)))## changes all missings from 2015 onwards to 0s



date_occurred_panel <- date_occurred_panel %>% 
  mutate(weekday = wday(date, label = T)) ## gets the weekday label


## Ferrum College is missing data. only have september 2015 - 2019
## Delaware state only has data from 2017 - onwards
## Texas Austin is missing jan/feb 2016
## NC state is missing data from August 2014 and before
date_occurred_panel <- date_occurred_panel %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape",
                  "report_lag_alc", "report_lag_sex", "report_lag_drug", "report_lag_theft", "report_lag_rob"), ~ifelse(
    year == 2015 & month < 9 & university == "Ferrum College",
    NA, .
  ))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape",
                  "report_lag_alc", "report_lag_sex", "report_lag_drug", "report_lag_theft", "report_lag_rob"), ~ifelse(
    year <= 2016 & university == "Delaware State University",
    NA, .
  ))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape",
                  "report_lag_alc", "report_lag_sex", "report_lag_drug", "report_lag_theft", "report_lag_rob"), ~ifelse(
    year== 2016 & (month == 1 | month == 2) & university == "The University of Texas at Austin",
    NA, .
  ))) %>% 
  mutate(across(c("alcohol_offense", "sexual_assault", "theft", "drug_offense","robbery_burglary", "alcohol_offense_strict", "noise_offense", "rape",
                  "report_lag_alc", "report_lag_sex", "report_lag_drug", "report_lag_theft", "report_lag_rob"), ~ifelse(
    year== 2014 & (month == 1 | month == 2 | month == 3 | month == 4 | month == 5 | month == 6 | month == 7| month == 8) & university == "North Carolina State University at Raleigh",
    NA, .
  )))



# Pulling in school closure data ------------------------------------------

##### Last Step: pulling in the closure data and merging it with this final_panel
closures <- read_xlsx("Data/closure_spreadsheet_final_2019.xlsx") %>% 
  janitor::clean_names() %>% 
  select(university, date, deadline, date2, deadline2,
         university_enacted_1, university_enacted_2, reason1, reason2) %>% 
  rename("closure_1" = date,
         "closure_1_end" = deadline,
         "closure_2" = date2,
         "closure_2_end" = deadline2)



date_occurred_panel <- date_occurred_panel %>% 
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


## now finally creating the columns for "proportion of crimes with reporting lag"
date_occurred_panel %>% 
  select(alcohol_offense, report_lag_alc) %>% 
  modelsummary::datasummary_skim()

date_occurred_panel <- date_occurred_panel %>% 
  mutate(proportion_alc_lag = ifelse(alcohol_offense == 0, 0, report_lag_alc/alcohol_offense),
         proportion_sex_lag = ifelse(sexual_assault == 0, 0, report_lag_sex/sexual_assault),
         proportion_drug_lag = ifelse(drug_offense == 0, 0, report_lag_drug/drug_offense),
         proportion_theft_lag = ifelse(theft == 0, 0, report_lag_theft/theft),
         proportion_rob_lab = ifelse(robbery_burglary == 0, 0, report_lag_rob/robbery_burglary)) 

lag_means <- date_occurred_panel %>% 
  ungroup() %>% 
  summarize(across(c(proportion_sex_lag, proportion_alc_lag), ~mean(.,na.rm = T), .names = "{.col}_mean"))
row_means <- tribble(~term , ~sex_lag, ~alc_lag,
                     "Mean of Dependent Variable",lag_means[[1]], lag_means[[2]])

attr(row_means, 'position') <- c(4)

lag_regression <- date_occurred_panel %>% 
  group_by(university, semester_number) %>% 
  mutate(uni_semester = cur_group_id()) %>% 
  ungroup() %>% 
  feols(c(proportion_sex_lag, proportion_alc_lag) ~ treatment |
          uni_semester + weekday, cluster = ~university, data = .) 
names(lag_regression) <- c("Sexual Assault", "Alcohol Offense")

lag_regresion_table <-  modelsummary::modelsummary(lag_regression,stars = T,gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
                             coef_map = c("treatment" = "Moratorium",
                                          "proportion_sex_lag" = "Proportion Reported w/ Lag (Sex)",
                                          "frac_undergrad_black" = "Fraction Undergrad Black",
                                          "frac_undergrad_asian" = "Fraction Undergrad Asian",
                                          "frac_undergrad_hispanic_latino" = "Fraction Undergrad Hispanic",
                                          "graduation_rate_total_cohort" = "Graduation Rate",
                                          "uni_semester" = "University by Semester"),
                             title = "Differences in reporting between moratorium and non-moratorium days.",
                             notes = list("The dependent variable is the proportion of offenses that are reported with a lag.",
                                          "A lag is defined as when date reported is more than 3 days later than the date occurred.",
                             "Observations are based off of a subset of the sample due to data constraints on the date reported."),
                             add_rows = row_means) %>% 
  kableExtra::add_header_above(c(" " = 1, "Proportion Reported with a Lag" = 2))


