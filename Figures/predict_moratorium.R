## Purpose of script: predicting if moratorium occurs or not in death universities
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-10
##

library(tidyverse)
library(readxl)

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv",
                        guess_max = 50000)
death_sheet <- read_csv("Data/death_universities_panel.csv")
ipeds <- read_csv("Created Data/IPEDS/ipeds_final.csv") %>% 
  filter(year > 2013)


## restricts to only universities that have experienced a fraternity death
predict_data <- death_sheet %>% 
  left_join(ipeds) %>% 
  filter(university %in% daily_crime$university) 

predict_data <- predict_data %>% 
  fastDummies::dummy_cols(select_columns = c("control_of_institution", "calendar_system")) 
predict_data %>% 
  colnames()
predict_data %>% 
  feols(moratorium ~ total_price_oncampus_oos  + total_price_oncampus_is +
          total_price_offcampus_is + total_price_offcampus_oos +
          frac_admitted_total + fulltime_retention_rate + undergraduate_enrollment + fulltime_undergraduate_enrollment +
          frac_total_asian + frac_total_black + frac_total_hispanic_latino + frac_ftime_undergrad_award_any_financial_aid +
          frac_ftime_undergrad_award_student_loans +
          frac_core_revenues_gov_contracts + frac_core_revenues_state_apro + frac_core_revenues_priv_gifts_grants +
          `control_of_institution_Private not-for-profit` +
          graduation_rate_total_cohort + death| year + university,
        cluster = ~university, data = .) %>% summary()
predict_data %>% 
  select(1:50) %>% 
  datasummary_skim()
