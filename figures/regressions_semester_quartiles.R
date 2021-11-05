## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-11-04
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)


if (!exists("semester_level")) {
  semester_level <- read_csv("created_data/xmaster_data/semester_level.csv")
}

if (!exists("semester_level_weekdays")){
  semester_level_weekdays <- read_csv("created_data/xmaster_data/semester_level_weekdays.csv")
}

if (!exists("semester_level_weekends")) {
  semester_level_weekends <- read_csv("created_data/xmaster_data/semester_level_weekends.csv")
  
}


semester_level_weekends %>% 
  feols(c(alcohol_offense_per25, drug_offense_per25, sexual_assault_per25) ~semester_before_dose_indicator + 
          treatment:fall_semester + treatment:spring_semester +semester_after_dose_indicator| university_by_semester_number + semester_number,
        cluster = ~university, data = .)
semester_level_weekends %>% 
  feols(c(alcohol_offense_per25, drug_offense_per25, sexual_assault_per25) ~semester_before_dose_indicator + treatment :quartile_1 + treatment:quartile_2 +
          treatment:quartile_3 + treatment:quartile_4 +semester_after_dose_indicator| university_by_semester_number + semester_number,
        cluster = ~university, data = .)
semester_level %>% 
  count(semester_number, date)
