## Purpose of script: appends the nibrs data to all the necessary data sets and creates master data for NIBRS
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-05
##

library(tidyverse)
library(lubridate)


# loading in data ---------------------------------------------------------

## be sure to run the aggregate file first!!!!!
nibrs <- read_csv("created_data/nibrs/nibrs_aggregated.csv")

closures <- readxl::read_excel("data/closure_spreadsheet_final_2019.xlsx") %>% 
  janitor::clean_names()

## pulling in the academic calendars
academic_calendars <- readxl::read_excel("data/academic_calendars_ori.xlsx") %>% janitor::clean_names()

nonschool_oris <- academic_calendars %>% 
  filter(ori_type == "nonschool") %>% 
  pull(ori)


# importing the arrest data -------------------------------------------------
group_b <- read_csv("created_data/nibrs/group_b_arrests.csv")

group_b <- group_b %>% 
  mutate(alcohol_arrest = ifelse(ucr_arrest_offense_code == "driving under the influence" |
                                   ucr_arrest_offense_code == "drunkenness" |
                                   ucr_arrest_offense_code == "liquor law violations", 1, 0)) %>% 
  mutate(college_aged = ifelse(age_of_arrestee >=17 & age_of_arrestee <= 22, "college_aged", "not_college")) %>% 
  group_by(arrest_date, ori, college_aged) %>% 
  summarize(alcohol_arrest = sum(alcohol_arrest,na.rm = T)) %>% ungroup() %>% 
  pivot_wider( names_from = college_aged, values_from = alcohol_arrest) %>% 
  select( -`NA`) %>% 
  rename(alcohol_arrest_college_aged = college_aged, alcohol_arrest_not_college_aged = not_college) %>% 
  mutate(across(-c(arrest_date, ori), ~ifelse(is.na(.), 0, .)))


### Note that due to this following graph that we must omit some of the following ORI due to reporting issues.
# group_b %>% 
#   rowwise() %>% 
#   mutate(alcohol_offense_total = sum(alcohol_arrest_college_aged, alcohol_arrest_not_college_aged, na.rm = T)) %>% 
#   filter(!(ori %in% c("IN0530100", "KS0230100", "MO0100200", "MS0360100", "NC0740900", "NC0921600", "TX1050100", "TX1050300", "VA0940400", "WV0060200",
#                       "SC0390200", "TXDPD0000", "VA0940400"))) %>% ## ones to omit
#   ggplot(aes(arrest_date, alcohol_offense_total)) +
#   geom_path() +
#   facet_wrap(~ori,scales = "free_y")



### Omitting the following for alcohol arrests::
## c("IN0530100", "KS0230100", "MO0100200", "MS0360100", "NC0740900", "NC0921600", "TX1050100", "TX1050300", "VA0940400", "WV0060200",
## "SC0390200", "TXDPD0000", "VA0940400")
  


# matching NIBRS to the ORI chosen ----------------------------------------

group_b <- group_b %>% 
  rowwise() %>% 
  mutate(alcohol_offense_total = sum(alcohol_arrest_college_aged, alcohol_arrest_not_college_aged, na.rm = T)) %>% 
  filter(!(ori %in% c("IN0530100", "KS0230100", "MO0100200", "MS0360100", "NC0740900", "NC0921600", "TX1050100", "TX1050300", "VA0940400", "WV0060200",
                      "SC0390200", "TXDPD0000", "VA0940400")))


consistent_ori <- group_b %>% distinct(ori) %>% pull()

nibrs_agg_schools <- nibrs %>% 
  filter(ori %in% consistent_ori) %>% 
  group_by(ori, incident_date, .drop = F) %>% 
  summarize(across(c(rape, sexual_assault_object, fondling, rape_statutory,
                     theft, college_age_rape,
                     starts_with("victim_"),college_age_sexual_assault_object, college_age_rape_statutory, 
                     college_age_fondle) , ~sum(.,na.rm = T))) %>% 
  mutate(ori_nonschool = ifelse(ori %in% nonschool_oris, 1, 0))




nibrs_full <- nibrs_agg_schools %>%
  left_join(group_b, by = c("ori", "incident_date" = "arrest_date"))


# create a full yearly panel for every school -----------------------------

# Creating Academic Calendar panels by semester ---------------------------

## moving all calendar dates to start in 2014
academic_calendars <- academic_calendars %>% 
  filter(ori %in% consistent_ori) %>% 
  separate(fall_start, into = c("fall_start"), sep = "\\s", extra = "merge") %>% 
  separate(fall_end, into = "fall_end", sep = "\\s") %>% 
  separate(spring_start, into = "spring_start", sep = "\\s") %>% 
  separate(spring_end, into = "spring_end", sep = "\\s") %>% 
  mutate(across(matches("^f|^s"), ~str_replace(., "\\d\\d\\d\\d", "2014"))) %>% 
  mutate(across(c(fall_start, fall_end, spring_start,spring_end), ~lubridate::ymd(.))) 


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
    group_by(ori) %>% 
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
    group_by(ori) %>% 
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



# mergining the panel with the crime data ---------------------------------

nibrs_panel_all_ori <- panel %>% 
  left_join(nibrs_full, by = c("date" = "incident_date", "ori" = "ori")) %>% 
  ungroup() %>% 
  mutate(across(c(4:21), ~ifelse(is.na(.), 0, .))) %>% 
  left_join(academic_calendars, by = c("ori"))


# creating the treatment variable and replacing NA with 0 for day --------

nibrs_panel_all_ori <- nibrs_panel_all_ori %>% 
  left_join(closures, by = c("university"="university")) %>% 
  rename("closure_1" = date.y,
         "closure_1_end" = deadline,
         "closure_2" = date2,
         "closure_2_end" = deadline2,
         "closure_3" = date3,
         "closure_3_end" = deadline3) %>% 
  rename("date" = "date.x") %>% 
  mutate(treatment = case_when(
    (!is.na(closure_1) & !is.na(closure_1_end)) & (date >= closure_1 & date < closure_1_end) ~ 1,
    (!is.na(closure_2) & !is.na(closure_2_end)) & (date >= closure_2 & date < closure_2_end) ~ 1,
    (!is.na(closure_3) & !is.na(closure_3_end) & (date >= closure_3 & date < closure_3_end)) ~ 1,
    TRUE ~as.double(0)
  ))



# adding in football games ------------------------------------------------

football_games <- read_csv("created_data/xmaster_data/football_final.csv") %>% 
  filter(school %in% nibrs_panel_all_ori$university)

nibrs_panel_all_ori <- nibrs_panel_all_ori %>% 
  left_join(football_games, by = c("university" = "school", "date" = "game_date"))



# adding fixed effects ----------------------------------------------------

library(timeDate)
veterans <- as.character(timeDate::holiday(2014:2019, "USVeteransDay")) 
thanksgiving <- as.character(timeDate::holiday(2014:2019, "USThanksgivingDay"))  
labor <- as.character(timeDate::holiday(2014:2019, "USLaborDay"))
halloween <- c("2014-10-31", "2015-10-31", "2016-10-31", "2017-10-31", "2018-10-31", "2019-10-31")
mlk <- as.character(timeDate::holiday(2014:2019, "USMLKingsBirthday"))
detach("package:timeDate", unload = TRUE)

nibrs_panel_all_ori <- nibrs_panel_all_ori %>% 
  mutate(labor = ifelse(date %in% lubridate::as_date(labor), 1, 0)) %>% 
  mutate(thanksgiving = ifelse(date %in% lubridate::as_date(thanksgiving), 1, 0)) %>% 
  mutate(halloween = ifelse(date %in% lubridate::as_date(halloween), 1, 0)) %>% 
  mutate(mlk = ifelse(date %in% lubridate::as_date(mlk), 1, 0)) %>% 
  mutate(veterans = ifelse(date %in% lubridate::as_date(veterans), 1, 0)) %>% 
  mutate(holiday = ifelse(labor == 1 | thanksgiving == 1 | halloween == 1 | mlk == 1 | veterans == 1, 1, 0))

nibrs_panel_all_ori <- nibrs_panel_all_ori %>% 
  mutate(academic_year = case_when(
    semester_number == 1 ~ 1, ## 2014 spring
    semester_number == 2 | semester_number == 3 ~ 2, ## 2015 
    semester_number == 4 | semester_number == 5 ~ 3,
    semester_number == 6 | semester_number == 7 ~ 4,
    semester_number == 8 | semester_number == 9 ~ 5,
    semester_number == 10 | semester_number == 11 ~ 6,
    semester_number == 12 ~7 ## 2019 fall
  )) %>% 
  mutate(spring_semester = ifelse(semester_number %% 2 == 0,1, 0)) %>% 
  mutate(game_occurred = ifelse(is.na(game_occurred), 0, game_occurred)) %>% 
  rename("alcohol_arrest_total" = "alcohol_offense_total") %>% 
  mutate(day_of_week = lubridate::wday(date,label = T)) %>% 
  mutate(year = lubridate::year(date), month = lubridate::month(date), week = lubridate::week(date)) %>% 
  group_by(month, ori) %>% 
  mutate(ori_by_month = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(ori, academic_year) %>% 
  mutate(ori_by_academic_year = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(ori, year) %>% 
  mutate(ori_by_year = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(ori, academic_year, week) %>% 
  mutate(ori_by_academic_year_by_week = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(ori, week, month) %>% 
  mutate(ori_by_month_by_week = cur_group_id()) %>% 
  ungroup %>% 
  group_by(ori, week) %>% 
  mutate(ori_by_week = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(ori, year, week) %>% 
  mutate(ori_by_year_by_week = cur_group_id()) %>% 
  ungroup() %>% 
  group_by(ori, academic_year, spring_semester) %>% 
  mutate(ori_by_academic_year_by_semester = cur_group_id()) %>% 
  ungroup()


# adding IPEDS ------------------------------------------------------------

ipeds <- read_csv("created_data/ipeds/ipeds_final.csv") %>% 
  filter(year > 2013)

nibrs_panel_all_ori <- nibrs_panel_all_ori %>% 
  left_join(ipeds, by = c("university", "year"))


# per25 enrollment and sum sexual assault ---------------------------------

nibrs_panel_all_ori <- nibrs_panel_all_ori %>% 
  rowwise() %>% 
  mutate(college_age_sexual_assault = sum(college_age_rape, college_age_rape_statutory, college_age_fondle, college_age_sexual_assault_object, na.rm = T),
         sexual_assault = sum(rape, rape_statutory, fondling, sexual_assault_object, na.rm = T))

nibrs_panel_all_ori <- nibrs_panel_all_ori %>% 
  mutate(across(c(college_age_sexual_assault, college_age_rape, college_age_rape_statutory, college_age_fondle, college_age_sexual_assault_object,
                  rape, rape_statutory, fondling, sexual_assault_object, alcohol_arrest_college_aged, sexual_assault, 
                  alcohol_arrest_not_college_aged, alcohol_arrest_total), ~ (. /total_enrollment) * 25000, .names = "{.col}_per25"))


# saving data -------------------------------------------------------------

write_csv(nibrs_panel_all_ori, file = "created_data/xmaster_data/nibrs_final.csv")







