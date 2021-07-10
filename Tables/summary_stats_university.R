
## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-16
##

library(modelsummary)
library(tidyverse)
library(kableExtra)

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv")

university_characteristics <- daily_crime %>% 
  mutate(private = ifelse(control_of_institution != "Public", 1, 0)) %>% 
  datasummary((`Total Enrollment` = total_enrollment) + 
                (`Total Undergrad Enrollment`=undergraduate_enrollment) + 
                (`Fraction Asian` = frac_undergrad_asian) +
                 (`Fraction Black`= frac_undergrad_black) + 
                (`Fraction Hispanic` = frac_undergrad_hispanic_latino) +
                (`Fraction White` = frac_undergrad_white) +
                 (`Graduation Rate` = graduation_rate_total_cohort) + 
                (`SAT Math 75` = sat_math_75) +
                ( `SAT Reading 75` = sat_reading_75) +
                (`Fraction Admitted` = frac_admitted_total) +
                (`Fraction Private`=private)~ (Mean + SD + Median + Min + Max), data = .,
              title = "University summary statistics of the 53 universities") %>% 
  add_indent(c(2:6)) %>% 
  add_indent(c(3:6))

