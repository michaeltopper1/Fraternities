## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-01-27
##

library(tidyverse)
library(modelsummary)
library(kableExtra)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv") %>% 
    filter(university %in% ifc::moratorium_schools())
}



summary_stats_university <- daily_crime %>%
  mutate(private = ifelse(control_of_institution != "Public", 1, 0)) %>% 
  datasummary((`Total Enrollment` = total_enrollment) + 
                (`Total Undergrad Enrollment`=undergraduate_enrollment) + 
                (`Fraction Asian` = frac_undergrad_asian) +
                (`Fraction Black`= frac_undergrad_black) + 
                (`Fraction Hispanic` = frac_undergrad_hispanic_latino) +
                (`Fraction White` = frac_undergrad_white) +
                (`Graduation Rate` = graduation_rate_total_cohort) + 
                (`Fraction Admitted` = frac_admitted_total) +
                (`Fraction Private`=private) ~ (Mean + SD + Median + Min + Max ), data = .,
              title = "Summary Statistics of the Universities in the Sample.") 


moratoriums <- ifc::moratorium_lengths()
moratoriums <- moratoriums %>% 
  group_by(university) %>% 
  mutate(moratorium_id = n()) %>% 
  ungroup()
moratorium_summary <- moratoriums %>% 
  datasummary((`Number of Moratoriums per-University` = moratorium_id) +(`Length of Moratoriums` = length_moratorium) ~ (Mean + SD + Median + Min + Max ), data = .,
              title = "Summary Statistics of Moratoriums") 
