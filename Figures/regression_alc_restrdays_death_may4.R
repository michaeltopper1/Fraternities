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

death_universities <- ifc::death_universities()
## filtering for weekends
daily_crime <- daily_crime %>% 
  filter(university %in% death_universities) %>% 
  mutate(across(c(ftime_total_undergrad, total_undergrad_asian,
                  total_undergrad_black, total_undergrad_hispanic),
                ~ ./total_students_undergrad))  %>% 
  filter(weekday == "Sat" | weekday == "Sun" | weekday == "Fri")
weekly_crime <- weekly_crime %>% 
  filter(university %in% death_universities) %>% 
  mutate(across(c(ftime_total_undergrad, total_undergrad_asian,
                  total_undergrad_black, total_undergrad_hispanic),
                ~ ./total_students_undergrad)) 
per1000_d <- daily_crime %>% 
  feols(alcohol_offense_per1000 ~ treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          university + month + year + weekday, cluster = ~university, data = .) 
pois_d <- daily_crime %>% 
  fepois(alcohol_offense ~ treatment + ftime_total_undergrad + 
           total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
           university + month + year + weekday, cluster = ~university, data = .) 
per1000_d_unimonth <- daily_crime %>% 
  feols(alcohol_offense_per1000 ~ treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year + weekday, cluster = ~university, data = .) 
pois_d_unimonth <- daily_crime %>% 
  fepois(alcohol_offense ~ treatment + ftime_total_undergrad + 
           total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
           uni_month + year + weekday, cluster = ~university, data = .) 



per1000_w <- weekly_crime %>% 
  feols(alcohol_offense_per1000 ~ treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          university + month + year, cluster = ~university, data = .) 
pois_w <- weekly_crime %>% 
  fepois(alcohol_offense ~ treatment + ftime_total_undergrad + 
           total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
           university + month + year, cluster = ~university, data = .) 
per1000_w_unimonth <- weekly_crime %>% 
  feols(alcohol_offense_per1000 ~ treatment + ftime_total_undergrad + 
          total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
          uni_month + year, cluster = ~university, data = .) 
pois_w_unimonth <- weekly_crime %>% 
  fepois(alcohol_offense ~ treatment + ftime_total_undergrad + 
           total_undergrad_black + total_undergrad_asian + total_undergrad_hispanic + graduation_rate_total_cohort_|
           uni_month + year, cluster = ~university, data = .) 


alc_models_restricted <- list("OLS - Per 1000 Students" = per1000_d, "OLS - Per 1000 Students" = per1000_d_unimonth,
                              "Poisson" = pois_d,"Poisson" = pois_d_unimonth,
                              "OLS - Per 1000 Students"= per1000_w,"OLS - Per 1000 Students"= per1000_w_unimonth,
                              "Poisson" = pois_w, "Poisson" = pois_w_unimonth)
alc_ols_restricted_death <- modelsummary(alc_models_restricted, stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log|R2 Within',
                                   coef_map = c("treatment" = "Moratorium",
                                                "ftime_total_undergrad" = "Fraction Full-time Undergrad",
                                                "total_undergrad_black" = "Fraction Undergrad Black",
                                                "total_undergrad_asian" = "Fraction Undergrad Asian",
                                                "total_undergrad_hispanic" = "Fraction Undergrad Hispanic",
                                                "graduation_rate_total_cohort_" = "Graduation Rate"),
                                   output = "kableExtra",title = "Effect of fraternity moratoria on alcohol offenses restricting to only weekends") %>% 
  add_header_above(c(" " = 1, "Daily Level (Fri/Sat/Sun)" = 4, "Weekly Level (Fri/Sat/Sun)" = 4)) %>% 
  add_footnote("Poisson regressions are based on counts and not per-1000-students.") %>% 
  add_footnote("Fixed effects of uni_month mean university-by-calendar-month.")