## Purpose of script: appends the nibrs data to all the necessary data sets and creates master data for NIBRS
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-05
##

library(tidyverse)
library(lubridate)


# loading in data ---------------------------------------------------------


nibrs <- read_csv("Created Data/nibrs/nibrs_aggregated.csv")

closures <- readxl::read_excel("Data/closure_spreadsheet_final_2019.xlsx") %>% 
  janitor::clean_names()


# choosing which ORI i want - ORI lindo/ ORI is schools ORI lindo  --------

closures_schools_only <- closures %>% 
  filter(!is.na(ori_9)) %>% 
  pivot_longer(c(ori_9, ori_9_lindo), values_to = "ori", names_to = "ori_type") %>% 
  relocate(ori_type, ori)

closures_nonschools_only <- closures %>% 
  filter(!is.na(ori_9)) %>% 
  pivot_longer(c(ori_9_lindo2), values_to = "ori", names_to = "ori_type") %>% 
  relocate(ori_type, ori)

closures_schools <- closures %>%
  filter(!is.na(ori_9))

closures_schools <- closures_schools %>% 
  pivot_longer(c(ori_9, ori_9_lindo2), values_to = "ori", names_to = "ori_type") %>% 
  relocate(ori_type, ori)



# matching NIBRS to the ORI chosen ----------------------------------------


nibrs_agg_schools <- nibrs %>% 
  left_join(closures_schools, by= c("ori" = "ori")) %>% 
  filter(!is.na(university)) %>% 
  group_by(university, incident_date, .drop = F) %>% 
  summarize(across(c(rape, sexual_assault_object, fondling, rape_statutory,
                   theft, college_age_rape,
                   starts_with("victim_"),college_age_sexual_assault_object, college_age_rape_statutory, 
                   college_age_fondle) , ~sum(.,na.rm = T))) 

nibrs_schools_only <- nibrs %>% 
  left_join(closures_schools_only,  by = c("ori" = "ori")) %>% 
  filter(!is.na(university)) %>% 
  group_by(university, incident_date, .drop = F) %>% 
  summarize(across(c(rape, sexual_assault_object, fondling, rape_statutory,
                     theft, college_age_rape,
                     starts_with("victim_"),college_age_sexual_assault_object, college_age_rape_statutory, 
                     college_age_fondle) , ~sum(.,na.rm = T))) 

nibrs_nonschools_only <- nibrs %>% 
  left_join(closures_nonschools_only,  by = c("ori" = "ori")) %>% 
  filter(!is.na(university)) %>% 
  group_by(university, incident_date, .drop = F) %>% 
  summarize(across(c(rape, sexual_assault_object, fondling, rape_statutory,
                     theft, college_age_rape,
                     starts_with("victim_"),college_age_sexual_assault_object, college_age_rape_statutory, 
                     college_age_fondle) , ~sum(.,na.rm = T))) 

# create a full yearly panel for every school -----------------------------

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

reporting_universities <- c("Arkansas State University-Main Campus",
                            "California Polytechnic State University-San Luis Obispo",
                            "Clemson University",
                            "College of Charleston",
                            "Marshall University",
                            "Murray State University",
                            "Ohio State University-Main Campus",
                            "Ohio University-Main Campus",
                            "University of Iowa",
                            "University of Kansas",
                            "Washington State University",
                            "West Virginia University",
                            "University of Vermont",
                            "University of Virginia-Main Campus")


# mergining the panel with the crime data ---------------------------------

nibrs_panel_all_ori <- panel %>% 
  left_join(nibrs_agg_schools, by = c("date" = "incident_date", "university" = "university")) %>% 
  filter(university %in% reporting_universities) 

nibrs_panel_schools_ori <- panel %>% 
  left_join(nibrs_schools_only, by = c("date" = "incident_date", "university" = "university")) %>% 
  filter(university %in% reporting_universities) 

nibrs_panel_nonschools_ori <- panel %>% 
  left_join(nibrs_nonschools_only, by = c("date" = "incident_date", "university" = "university")) %>% 
  filter(university %in% reporting_universities) 


# creating the treatment variable and replacing NA with 0 for day --------

nibrs_panel_all_ori <- nibrs_panel_all_ori %>% 
  left_join(closures, by = c("university"="university")) %>%
  mutate(across(c(rape, sexual_assault_object,
                fondling, rape_statutory, theft, college_age_rape,
                college_age_sexual_assault_object, college_age_rape_statutory, 
                college_age_fondle), ~ifelse(is.na(.), 0, .))) %>% 
  rename("date_of_calendar" = "date.x","date" = "date.y") %>% 
  rename("closure_1" = date,
         "closure_1_end" = deadline,
         "closure_2" = date2,
         "closure_2_end" = deadline2) %>% 
  rename("date" = "date_of_calendar") %>% 
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

nibrs_panel_schools_ori <- nibrs_panel_schools_ori %>% 
  left_join(closures, by = c("university"="university")) %>%
  mutate(across(c(rape, sexual_assault_object,
                  fondling, rape_statutory, theft, college_age_rape,
                  college_age_sexual_assault_object, college_age_rape_statutory, 
                  college_age_fondle), ~ifelse(is.na(.), 0, .))) %>% 
  rename("date_of_calendar" = "date.x","date" = "date.y") %>% 
  rename("closure_1" = date,
         "closure_1_end" = deadline,
         "closure_2" = date2,
         "closure_2_end" = deadline2) %>% 
  rename("date" = "date_of_calendar") %>% 
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


nibrs_panel_nonschools_ori <- nibrs_panel_nonschools_ori %>% 
  left_join(closures, by = c("university"="university")) %>%
  mutate(across(c(rape, sexual_assault_object,
                  fondling, rape_statutory, theft, college_age_rape,
                  college_age_sexual_assault_object, college_age_rape_statutory, 
                  college_age_fondle), ~ifelse(is.na(.), 0, .))) %>% 
  rename("date_of_calendar" = "date.x","date" = "date.y") %>% 
  rename("closure_1" = date,
         "closure_1_end" = deadline,
         "closure_2" = date2,
         "closure_2_end" = deadline2) %>% 
  rename("date" = "date_of_calendar") %>% 
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



# saving data -------------------------------------------------------------

write_csv(nibrs_panel_all_ori, file = "Created Data/nibrs/final_panel_all.csv")
write_csv(nibrs_panel_schools_ori, file = "Created Data/nibrs/final_panel_schools.csv")
write_csv(nibrs_panel_nonschools_ori, file = "Created Data/nibrs/final_panel_nonschools.csv")






