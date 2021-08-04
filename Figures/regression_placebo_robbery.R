## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-07-19
##

library(tidyverse)
library(lubridate)
library(fixest)
library(kableExtra)
library(modelsummary)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv")
}

daily_crime_weekdays<- daily_crime %>% 
  filter(weekday != "Fri" & weekday != "Sat" & weekday != "Sun")

daily_crime_weekends <- daily_crime %>% 
  filter(weekday == "Fri" | weekday == "Sat" | weekday == "Sun")

robbery_ols <- daily_crime %>% 
  feols(robbery_burglary_per25 ~ treatment |
          uni_semester + weekday, cluster = ~university, data = .)

robbery_p <- daily_crime %>% 
  fepois(robbery_burglary_per25 ~ treatment |
          uni_semester + weekday, cluster = ~university, data = .)



full_means <- daily_crime %>% 
  summarize(robbery_25 = mean(robbery_burglary_per25, na.rm = T),
            robbery_mean = mean(robbery_burglary, na.rm = T)) %>% 
  mutate(across(everything(), ~round(.,4)))

row_means <- tribble(~term, ~rob, ~rob_p,
                     'Mean of Outcome',full_means[[1]], weekend_means[[1]])
attr(row_means, 'position') <- c(4)
robbery_regs <-  list("OLS" = robbery_ols,
                      "Poisson" = robbery_p)
robbery_table <- modelsummary(robbery_regs, stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
             coef_map = c("treatment" = "Moratorium",
                          "ftime_total_undergrad" = "Fraction Full-time Undergrad",
                          "frac_undergrad_black" = "Fraction Undergrad Black",
                          "frac_undergrad_asian" = "Fraction Undergrad Asian",
                          "frac_undergrad_hispanic_latino" = "Fraction Undergrad Hispanic",
                          "graduation_rate_total_cohort" = "Graduation Rate",
                          "uni_semester" = "University by Semester"),
             title = "Effect of Fraternity Moratoriums on Robbery/Burglary",
             notes = list("Reports of robbery for ols regressions are per-25,000 students enrolled.",
                          "Poisson regressions are counts.",
             "Estimates include all days of the week (Mon-Sun)."),
             add_rows = row_means) %>% 
  add_header_above(c(" " = 1, "(1)" = 1, "(2)" = 1))



