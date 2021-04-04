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
model_1_drug <- summary(feols(drug_offense ~ treatment  +
                               total_undergrad_all + ftime_total_undergrad +
                               graduation_rate_total_cohort_ + 
                               total_undergrad_black + total_undergrad_asian + 
                               total_undergrad_hispanic  |
                               university + year_by_month_by_weekday,
                             data = daily_crime), cluster = ~university)
model_2_drug <- summary(feols(drug_offense ~ treatment  +
                               total_undergrad_all + ftime_total_undergrad +
                               graduation_rate_total_cohort_ + 
                               total_undergrad_black + total_undergrad_asian + 
                               total_undergrad_hispanic  |
                               university + weekday + year,
                             data = daily_crime), cluster = ~university)
model_3_drug <- summary(feols(drug_offense ~ treatment  +
                               total_undergrad_all + ftime_total_undergrad +
                               graduation_rate_total_cohort_ + 
                               total_undergrad_black + total_undergrad_asian + 
                               total_undergrad_hispanic  |
                               university_by_month + weekday,
                             data = daily_crime), cluster = ~university)
## Alcohol

model_1_theft <- summary(feols(theft ~ treatment  +
                                   total_undergrad_all + ftime_total_undergrad +
                                   graduation_rate_total_cohort_ + 
                                   total_undergrad_black + total_undergrad_asian + 
                                   total_undergrad_hispanic  |
                                   university + year_by_month_by_weekday,
                                 data = daily_crime), cluster= ~university)
model_2_theft <- summary(feols(theft ~ treatment  +
                                   total_undergrad_all + ftime_total_undergrad +
                                   graduation_rate_total_cohort_ + 
                                   total_undergrad_black + total_undergrad_asian + 
                                   total_undergrad_hispanic  |
                                   university + weekday + year,
                                 data = daily_crime, se= "cluster"), cluster = ~university)
model_3_theft <- summary(feols(theft ~ treatment  +
                                   total_undergrad_all + ftime_total_undergrad +
                                   graduation_rate_total_cohort_ + 
                                   total_undergrad_black + total_undergrad_asian + 
                                   total_undergrad_hispanic  |
                                   university_by_month + weekday,
                                 data = daily_crime, se= "cluster"), cluster = ~university)

models_drug_theft <- list("(1)" = model_1_drug, "(2)" = model_2_drug, "(3)" = model_3_drug,
                       "(1)"= model_1_theft, "(2)" =model_2_theft, "(3)" =model_3_theft)

models_drug_theft <- modelsummary(models_drug_theft, output = "kableExtra", stars  = T, coef_rename = c(
  "treatment" = "Moratorium",
  "total_undergrad_all" = "Total Undergrad Pop",
  "ftime_total_undergrad" = "Full Time Undergrad Pop",
  "graduation_rate_total_cohort_" = "Graduation Rate",
  "total_undergrad_black" = "Undergrad Black",
  "total_undergrad_asian" = "Undergrad Asian",
  "total_undergrad_hispanic" = "Undergrad Hispanic"
), title = "Effect of Fraternity Moratoria on Reports of Drug and Theft Offenses")

models_drug_theft <- models_drug_theft %>% 
  add_header_above(c(" " = 1,  "Daily Reports of Drug Offenses"= 3, "Daily Reports of Theft" = 3)) 



