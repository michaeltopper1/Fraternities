
## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-03-31
##

library(modelsummary)
library(tidyverse)
library(kableExtra)
daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv",
                        guess_max = 50000)
weekly_crime <- read_csv("Created Data/xMaster_data_2021/weekly_panel.csv",
                         guess_max = 50000)




university_characteristics <- daily_crime %>% 
  mutate(total_undergrad_white = total_undergrad_white_men + total_undergrad_white_women) %>% 
  mutate(private = ifelse(sector_of_institution == 2, 1, 0)) %>% 
  mutate(across(c(total_undergrad_black, total_undergrad_asian, total_undergrad_hispanic, total_undergrad_white),
                ~./total_students_undergrad)) %>% 
  datasummary((`Total Students` = total_students_all) + 
                (`Total Undergrad Students`=total_students_undergrad) + 
                (`Fraction Asian` = total_undergrad_asian) +
                 (`Fraction Black`= total_undergrad_black) + 
                (`Fraction Hispanic` = total_undergrad_hispanic) +
                (`Fraction White` = total_undergrad_white) +
                (`Fraction Full-time` =ftime_total_undergrad) +
                 (`Graduation Rate` = graduation_rate_total_cohort_) + 
                (`Fraction Private`=private)~ (Mean + SD + Median + Min + Max), data = .,
              title = "University Characteristics") %>% 
  add_indent(c(2:6)) %>% 
  add_indent(c(3:6))

