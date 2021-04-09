## Purpose of script: Hetereogenous effects by size of closure. I split by median length of closure. 
##
## Author: Michael Topper
##
## Date Last Edited: 2021-04-08
##

library(tidyverse)
library(fixest) 
library(modelsummary)
library(ifc)
library(lubridate)
library(kableExtra)

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel_nosummer.csv",
                        guess_max = 50000)
weekly_crime <- read_csv("Created Data/xMaster_data_2021/weekly_panel_nosummer.csv",
                         guess_max = 50000)

daily_crime <- daily_crime %>% 
  mutate(across(c(ftime_total_undergrad, total_undergrad_asian,
                  total_undergrad_black, total_undergrad_hispanic),
                ~ ./total_students_undergrad)) 
weekly_crime <- weekly_crime %>% 
  mutate(across(c(ftime_total_undergrad, total_undergrad_asian,
                  total_undergrad_black, total_undergrad_hispanic),
                ~ ./total_students_undergrad)) 

daily_crime <- daily_crime %>% 
  group_by(university) %>% 
  mutate(length_1= closure_1_end - closure_1, length_2 = closure_2_end - closure_2 ) %>% 
  extract(length_1, "length_1", "(^\\d{1,2})") %>% 
  extract(length_2, "length_2", "(^\\d{1,2})") %>% 
  mutate(across(starts_with("length"), ~as.double(.))) %>% 
  rowwise() %>% 
  mutate(avg_length = sum(length_1, length_2, na.rm = T)) %>% 
  mutate(avg_length = ifelse(avg_length > length_1, avg_length/2, avg_length)) %>% 
  ungroup() %>% 
  mutate(median_length = median(avg_length)) %>%
  mutate(long_closure = ifelse(avg_length >= median_length, 1, 0))


weekly_crime <- weekly_crime %>% 
  group_by(university) %>% 
  mutate(length_1= closure_1_end - closure_1, length_2 = closure_2_end - closure_2 ) %>% 
  extract(length_1, "length_1", "(^\\d{1,2})") %>% 
  extract(length_2, "length_2", "(^\\d{1,2})") %>% 
  mutate(across(starts_with("length"), ~as.double(.))) %>%
  rowwise() %>% 
  mutate(avg_length = sum(length_1, length_2, na.rm = T)) %>% 
  mutate(avg_length = ifelse(avg_length > length_1, avg_length/2, avg_length)) %>% 
  ungroup() %>% 
  mutate(median_length = median(avg_length)) %>% 
  mutate(long_closure = ifelse(avg_length >= median_length, 1, 0))

per100_alc_d <- daily_crime %>% 
  feols(alcohol_offense_per100 ~ treatment:long_closure + treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year + weekday, cluster = ~university, data = .) 
ihs_alc_d <- daily_crime %>% 
  feols(ihs_alcohol_offense ~ treatment:long_closure + treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year + weekday, cluster = ~university, data = .) 
per100_sex_d <- daily_crime %>% 
  feols(sexual_assault_per100 ~ treatment:long_closure + treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year + weekday, cluster = ~university, data = .) 
ihs_sex_d <- daily_crime %>% 
  feols(ihs_sexual_assault ~ treatment:long_closure + treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year + weekday, cluster = ~university, data = .) 


per100_alc_w <- weekly_crime %>% 
  feols(alcohol_offense_per100 ~ treatment:long_closure + treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year , cluster = ~university, data = .) 
ihs_alc_w <- weekly_crime %>% 
  feols(ihs_alcohol_offense ~ treatment:long_closure  + treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year , cluster = ~university, data = .) 
per100_sex_w <- weekly_crime %>% 
  feols(sexual_assault_per100 ~ treatment:long_closure + treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year , cluster = ~university, data = .) 
ihs_sex_w <- weekly_crime %>% 
  feols(ihs_sexual_assault ~ treatment:long_closure + treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year , cluster = ~university, data = .) 

hetero_closure <- list("Per 100k" = per100_alc_d,
                       "IHS" = ihs_alc_d,
                       "Per 100k" = per100_sex_d,
                       "IHS" = ihs_sex_d,
                       "Per 100k" = per100_alc_w,
                       "IHS" = ihs_alc_w,
                       "Per 100k" = per100_sex_w,
                       "IHS" = ihs_sex_w)
hetero_closure <- modelsummary(hetero_closure, stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log|R2 Within|R2 Ps',
             coef_map = c("treatment" = "Moratorium",
                          "ftime_total_undergrad" = "Fraction Full-time Undergrad",
                          "total_undergrad_black" = "Fraction Undergrad Black",
                          "total_undergrad_asian" = "Fraction Undergrad Asian",
                          "total_undergrad_hispanic" = "Fraction Undergrad Hispanic",
                          "graduation_rate_total_cohort_" = "Graduation Rate",
                          "treatment:long_closure" = "Moratorium x Long Closure"), title = "Difference in effects between long/short moratoria (split by median)") %>% 
  add_header_above(c(" " = 1, "Alcohol Offense" = 2, "Sexual Assault" = 2, "Alcohol Offense" = 2, "Sexual Assault" = 2)) %>% 
  add_header_above(c(" " = 1, "Daily Reports" = 4, "Weekly Reports" = 4)) %>% 
  row_spec(13:14, bold = T, color = "red")
  
