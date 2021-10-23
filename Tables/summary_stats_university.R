
## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-16
##

library(modelsummary)
library(tidyverse)
library(kableExtra)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv") %>% 
    filter(university %in% ifc::moratorium_schools())
}


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
                (`Fraction Private`=private) +
                (`Alcohol Offense` = alcohol_offense_per25) + 
                (`Sexual Assault` = sexual_assault_per25) +
                (`Drug Offense` = drug_offense_per25) +
                (`Robbery/Burglary` = robbery_burglary_per25) ~ (Mean + SD + Median + Min + Max), data = .,
              title = "Summary statistics of the 38 universities in the sample and outcomes used in analysis.",
              notes = 'Offenses are per-25000 students enrolled.') %>% 
  add_indent(c(2:6)) %>% 
  add_indent(c(3:6)) %>% 
  pack_rows("University Characteristics", 1, 11) %>% 
  pack_rows("Daily Crime Log Offenses", 12, 15)

