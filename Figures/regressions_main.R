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

sex <- daily_crime %>% 
  feols(sexual_assault_per25 ~ treatment |
          uni_semester + weekday, cluster = ~university, data = .)

alc <- daily_crime %>% 
  feols(alcohol_offense_per25 ~  treatment  |
          uni_semester + weekday, cluster = ~university, data = .)

sex_weekdays <- daily_crime_weekdays %>% 
  feols(sexual_assault_per25 ~ treatment |
          uni_semester + weekday, cluster = ~university, data = .)

alc_weekdays <- daily_crime_weekdays %>% 
  feols(alcohol_offense_per25 ~  treatment  |
          uni_semester + weekday, cluster = ~university, data = .)

sex_weekends <- daily_crime_weekends %>% 
  feols(sexual_assault_per25 ~ treatment |
          uni_semester + weekday, cluster = ~university, data = .)

alc_weekends <- daily_crime_weekends %>% 
  feols(alcohol_offense_per25 ~  treatment  |
          uni_semester + weekday, cluster = ~university, data = .)

full_means <- daily_crime %>% 
  summarize(alcohol_mean = mean(alcohol_offense_per25, na.rm = T),
            sex_mean = mean(sexual_assault_per25, na.rm = T)) %>% 
  mutate(across(everything(), ~round(.,4)))
weekend_means <- daily_crime_weekends %>% 
  summarize(alcohol_mean = mean(alcohol_offense_per25, na.rm = T),
            sex_mean = mean(sexual_assault_per25, na.rm = T)) %>% 
  mutate(across(everything(), ~round(.,4)))
weekday_means <- daily_crime_weekdays %>% 
  summarize(alcohol_mean = mean(alcohol_offense_per25, na.rm = T),
            sex_mean = mean(sexual_assault_per25, na.rm = T)) %>% 
  mutate(across(everything(), ~round(.,4)))

row_means <- tribble(~term, ~alc, ~alc_weeknd, ~alc_weekday, ~sex, ~sex_weekend, ~sex_weekday,
                     'Mean of Outcome',full_means[[1]], weekend_means[[1]], weekday_means[[1]], full_means[[2]], weekend_means[[2]], weekday_means[[2]])
attr(row_means, 'position') <- c(4)
main_regs <- list(
  "Full Sample" = alc,
  "Weekends" = alc_weekends,
  "Weekdays" = alc_weekdays,
  "Full Sample" = sex,
  "Weekends" = sex_weekends,
  "Weekdays"= sex_weekdays)

main_results_table <- modelsummary(main_regs, stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
             coef_map = c("treatment" = "Moratorium",
                          "ftime_total_undergrad" = "Fraction Full-time Undergrad",
                          "frac_undergrad_black" = "Fraction Undergrad Black",
                          "frac_undergrad_asian" = "Fraction Undergrad Asian",
                          "frac_undergrad_hispanic_latino" = "Fraction Undergrad Hispanic",
                          "graduation_rate_total_cohort" = "Graduation Rate",
                          "uni_semester" = "University by Semester"),
             title = "Effect of Fraternity Moratoriums on Sexual Assault and Alcohol Offenses",
             notes = "Reports of sexual assault and counts of alcohol offenses are per 25,000 students enrolled. 
             Full sample includes all days of the week (Mon-Sun), weekends include only Friday-Sunday, and weekdays include only Monday-Thursday.",
             add_rows = row_means) %>% 
  add_header_above(c(" " = 1, "Alcohol Offense" = 3, "Sexual Assault"= 3))





