## Purpose of script: Main Model with different fixed effects and controls
##
## Author: Michael Topper
##
## Date Last Edited: 2021-03-03
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

daily_crime <- read_csv("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/xMaster_data_2021/daily_crime_analysis.csv",
                        guess_max = 50000)

## NOTE THAT YOU SHOULD DELETE THIS WHEN OYU GET THE DATA FROM HERE AND FROM THE ANALYSIS CREATION FILE
daily_crime <- daily_crime %>% 
  filter(university != "Miami University-Oxford" & university != "Emory University") 


## Model 1: Sexual Assault
## Try with following fixed effects: university-by-weekday, university month-by-year-by-weekday, 
## Controls: graduation rate, total undergrads, ftime total undergrad, undergrad race
## Try with Poisson
model_1_robbery <- summary(feols(robbery_burglary ~ treatment  +
                                total_undergrad_all + ftime_total_undergrad +
                                graduation_rate_total_cohort_ + 
                                total_undergrad_black + total_undergrad_asian + 
                                total_undergrad_hispanic  |
                                university + year_by_month_by_weekday,
                              data = daily_crime), cluster = ~university)
model_2_robbery <- summary(feols(robbery_burglary ~ treatment  +
                                total_undergrad_all + ftime_total_undergrad +
                                graduation_rate_total_cohort_ + 
                                total_undergrad_black + total_undergrad_asian + 
                                total_undergrad_hispanic  |
                                university + weekday + year,
                              data = daily_crime), cluster = ~university)
model_3_robbery <- summary(feols(robbery_burglary ~ treatment  +
                                total_undergrad_all + ftime_total_undergrad +
                                graduation_rate_total_cohort_ + 
                                total_undergrad_black + total_undergrad_asian + 
                                total_undergrad_hispanic  |
                                university_by_month + weekday,
                              data = daily_crime), cluster = ~university)
## Alcohol

model_1_noise <- summary(feols(noise_offense ~ treatment  +
                                 total_undergrad_all + ftime_total_undergrad +
                                 graduation_rate_total_cohort_ + 
                                 total_undergrad_black + total_undergrad_asian + 
                                 total_undergrad_hispanic  |
                                 university + year_by_month_by_weekday,
                               data = daily_crime), cluster= ~university)
model_2_noise <- summary(feols(noise_offense ~ treatment  +
                                 total_undergrad_all + ftime_total_undergrad +
                                 graduation_rate_total_cohort_ + 
                                 total_undergrad_black + total_undergrad_asian + 
                                 total_undergrad_hispanic  |
                                 university + weekday + year,
                               data = daily_crime, se= "cluster"), cluster = ~university)
model_3_noise <- summary(feols(noise_offense ~ treatment  +
                                 total_undergrad_all + ftime_total_undergrad +
                                 graduation_rate_total_cohort_ + 
                                 total_undergrad_black + total_undergrad_asian + 
                                 total_undergrad_hispanic  |
                                 university_by_month + weekday,
                               data = daily_crime, se= "cluster"), cluster = ~university)

models_robbery_noise <- list("(1)" = model_1_robbery, "(2)" = model_2_robbery, "(3)" = model_3_robbery,
                          "(1)"= model_1_noise, "(2)" =model_2_noise, "(3)" =model_3_noise)

models_robbery_noise <- modelsummary(models_robbery_noise, output = "kableExtra", stars  = T, coef_rename = c(
  "treatment" = "Moratorium",
  "total_undergrad_all" = "Total Undergrad Pop",
  "ftime_total_undergrad" = "Full Time Undergrad Pop",
  "graduation_rate_total_cohort_" = "Graduation Rate",
  "total_undergrad_black" = "Undergrad Black",
  "total_undergrad_asian" = "Undergrad Asian",
  "total_undergrad_hispanic" = "Undergrad Hispanic"
), title = "Effect of Fraternity Moratoria on Reports of Robbery and Noise Offenses")

models_robbery_noise <- models_robbery_noise %>% 
  add_header_above(c(" " = 1,  "Daily Reports of Robbery/Burglary Offenses"= 3, "Daily Reports of Noise Complaints" = 3))



