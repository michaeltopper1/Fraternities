library(tidyverse)
library(fixest) 
library(modelsummary)
library(ifc)
library(lubridate)
library(kableExtra)


daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel_nosummer.csv",
                        guess_max = 50000) 
weekly_crime <- read_csv("Created Data/xMaster_data_2021/weekly_panel_weekends_nosummer.csv",
                         guess_max = 50000)


daily_crime <- daily_crime %>% 
  mutate(across(c(ftime_total_undergrad, total_undergrad_asian,
                  total_undergrad_black, total_undergrad_hispanic),
                ~ ./total_students_undergrad))  %>% 
  filter(weekday == "Sat" | weekday == "Sun" | weekday == "Fri")
weekly_crime <- weekly_crime %>% 
  mutate(across(c(ftime_total_undergrad, total_undergrad_asian,
                  total_undergrad_black, total_undergrad_hispanic),
                ~ ./total_students_undergrad)) 

per1000_d <- daily_crime %>% 
  feols(alcohol_offense_per1000 ~ treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month+ year + weekday, cluster = ~university, data = .) 
pois_d <- daily_crime %>% 
  fepois(alcohol_offense ~ treatment + ftime_total_undergrad + 
           total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
           uni_month + year + weekday, cluster = ~university, data = .) 


per1000_w <- weekly_crime %>% 
  feols(alcohol_offense_per1000 ~ treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year, cluster = ~university, data = .) 
pois_w <- weekly_crime %>% 
  fepois(alcohol_offense ~ treatment + ftime_total_undergrad + 
           total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
           uni_month + year, cluster = ~university, data = .) 


alc_models_restricted <- list("OLS - Per 1000 Students" = per1000_d, "Poisson" = pois_d,
                              "OLS - Per 1000 Students"= per1000_w,
                              "Poisson" = pois_w)
alc_ols_restricted_unimonth <- modelsummary(alc_models_restricted, stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log|R2 Within|R2 Ps',
                                   coef_map = c("treatment" = "Moratorium",
                                                "ftime_total_undergrad" = "Fraction Full-time Undergrad",
                                                "total_undergrad_black" = "Fraction Undergrad Black",
                                                "total_undergrad_asian" = "Fraction Undergrad Asian",
                                                "total_undergrad_hispanic" = "Fraction Undergrad Hispanic",
                                                "graduation_rate_total_cohort_" = "Graduation Rate"),
                                   output = "kableExtra",title = "Effect of fraternity moratoria on alcohol offenses restricting to only weekends: University-by-calendar-month fixed effects.") %>% 
  add_header_above(c(" " = 1, "Daily Level (Fri/Sat/Sun)" = 2, "Weekly Level (Fri/Sat/Sun)" = 2)) %>% 
  add_footnote("Poisson regressions are based on counts and not per-1000-students.") %>% 
  add_footnote("Fixed effects are university-by-calendar-month, year, and university. Weekday fixed effects included in daily levels.")

