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

##### GET RID
# daily_crime <- daily_crime %>%
#   filter(weekday != "Mon" & weekday != "Tue" & weekday != "Wed" & weekday != "Thu")

## Model 1: Sexual Assault
## Try with following fixed effects: university-by-weekday, university month-by-year-by-weekday, 
## Controls: graduation rate, total undergrads, ftime total undergrad, undergrad race
## Try with Poisson
model_1_sex <- summary(feols(sexual_assault ~ treatment  +
                          total_undergrad_all + ftime_total_undergrad +
                          graduation_rate_total_cohort_ + 
                          total_undergrad_black + total_undergrad_asian + 
                          total_undergrad_hispanic  |
                          university + year_by_month_by_weekday,
                          data = daily_crime), cluster = ~university)
model_2_sex <- summary(feols(sexual_assault ~ treatment  +
                      total_undergrad_all + ftime_total_undergrad +
                      graduation_rate_total_cohort_ + 
                      total_undergrad_black + total_undergrad_asian + 
                      total_undergrad_hispanic  |
                      university + weekday + year,
                     data = daily_crime), cluster = ~university)
model_3_sex <- summary(feols(sexual_assault ~ treatment  +
                      total_undergrad_all + ftime_total_undergrad +
                      graduation_rate_total_cohort_ + 
                      total_undergrad_black + total_undergrad_asian + 
                      total_undergrad_hispanic  |
                      university_by_month + weekday,
                     data = daily_crime), cluster = ~university)
## Alcohol

model_1_alcohol <- summary(feols(alcohol_offense ~ treatment  +
                       total_undergrad_all + ftime_total_undergrad +
                       graduation_rate_total_cohort_ + 
                       total_undergrad_black + total_undergrad_asian + 
                       total_undergrad_hispanic  |
                       university + year_by_month_by_weekday,
                     data = daily_crime), cluster= ~university)
model_2_alcohol <- summary(feols(alcohol_offense ~ treatment  +
                       total_undergrad_all + ftime_total_undergrad +
                       graduation_rate_total_cohort_ + 
                       total_undergrad_black + total_undergrad_asian + 
                       total_undergrad_hispanic  |
                       university + weekday + year,
                     data = daily_crime, se= "cluster"), cluster = ~university)
model_3_alcohol <- summary(feols(alcohol_offense ~ treatment  +
                       total_undergrad_all + ftime_total_undergrad +
                       graduation_rate_total_cohort_ + 
                       total_undergrad_black + total_undergrad_asian + 
                       total_undergrad_hispanic  |
                       university + month,
                     data = daily_crime, se= "cluster"), cluster = ~university)

models_sex_alc <- list("(1)" = model_1_sex, "(2)" = model_2_sex, "(3)" = model_3_sex,
                   "(1)"= model_1_alcohol, "(2)" =model_2_alcohol, "(3)" =model_3_alcohol)

models_sex_alc <- modelsummary(models_sex_alc, output = "kableExtra", stars  = T, coef_rename = c(
  "treatment" = "Moratorium",
  "total_undergrad_all" = "Total Undergrad Pop",
  "ftime_total_undergrad" = "Full Time Undergrad Pop",
  "graduation_rate_total_cohort_" = "Graduation Rate",
  "total_undergrad_black" = "Undergrad Black",
  "total_undergrad_asian" = "Undergrad Asian",
  "total_undergrad_hispanic" = "Undergrad Hispanic"
), title = "Effect of Fraternity Moratoria on Reports of Sexual Assault and Alcohol Offenses")

models_sex_alc <- models_sex_alc %>% 
  add_header_above(c(" " = 1,  "Daily Reports of Sexual Assault"= 3, "Daily Reports of Alcohol Offenses" = 3))


# model_1_sex <- feols(sexual_assault ~ treatment  +
#                       ftime_total_undergrad +
#                       graduation_rate_total_cohort_ + 
#                       total_undergrad_black + total_undergrad_asian + 
#                       total_undergrad_hispanic  |
#                        year_by_month_by_weekday + university,
#                       data = daily_crime, se = "cluster")
# summary(model_1_sex, cluster = ~university)
# summary(model_1_sex)
# 
# mod_1 <- summary(model_1_sex, cluster = ~university)
# modelsummary(mod_1)
