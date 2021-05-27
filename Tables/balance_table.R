## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-15
##
## Purpose of script: balance table - to be completed after all never treated are added in
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-16
##

library(tidyverse)
library(tidyverse)
library(fixest)
library(modelsummary)

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv")

daily_crime %>% 
  mutate(never_treated = ifelse(university %in% ifc::untreated_universities(), "No Moratorium", "Moratorium")) %>% 
  select(total_enrollment, undergraduate_enrollment, fulltime_retention_rate, frac_total_black,
         frac_total_asian, frac_total_white, frac_total_hispanic_latino,
         sat_math_75, sat_reading_75, graduation_rate_total_cohort, never_treated, sexual_assault_per25,
         alcohol_offense_per25, drug_offense_per25, theft_per25, robbery_burglary_per25) %>% 
  datasummary_balance(~never_treated, data = ., fmt = 3,
                      dinm_statistic = "p.value")
