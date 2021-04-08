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

per100_d <- daily_crime %>% 
  feols(alcohol_offense_per100 ~ treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year + weekday, cluster = ~university, data = .) 
ihs_d <- daily_crime %>% 
  feols(ihs_alcohol_offense ~ treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year + weekday, cluster = ~university, data = .) 


per100_w <- weekly_crime %>% 
  feols(alcohol_offense_per100 ~ treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year, cluster = ~university, data = .) 
ihs_w <- weekly_crime %>% 
  feols(ihs_alcohol_offense ~ treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year, cluster = ~university, data = .) 


alc_models <- list("Per 100k Students" = per100_d, "IHS(Alcohol Offense)" = ihs_d,
                   "Per 100k Students"= per100_w,
                   "IHS(Alcohol Offense)" = ihs_w)
alc_ols <- modelsummary(alc_models, stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log|R2 Within|R2 Ps',
             coef_map = c("treatment" = "Moratorium",
                          "ftime_total_undergrad" = "Fraction Full-time Undergrad",
                          "total_undergrad_black" = "Fraction Undergrad Black",
                          "total_undergrad_asian" = "Fraction Undergrad Asian",
                          "total_undergrad_hispanic" = "Fraction Undergrad Hispanic",
                          "graduation_rate_total_cohort_" = "Graduation Rate"),
             output = "kableExtra", title = "Effect of fraternity moratoria on alcohol offenses") %>% 
  add_header_above(c(" " = 1, "Daily Level" = 2, "Weekly Level" = 2))

  
