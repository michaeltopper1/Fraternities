## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-11-01
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

semester_level <- read_csv("created_data/xmaster_data/semester_level.csv")
semester_level_weekdays <- read_csv("created_data/xmaster_data/semester_level_weekdays.csv")
semester_level_weekends <- read_csv("created_data/xmaster_data/semester_level_weekends.csv")


# function for means ------------------------------------------------------

find_mean <- function(data, column) {
  column_mean <- data %>% 
    summarize(mean({{column}}, na.rm = T)) %>% 
    pull()
  return(column_mean)
}

add_means_alc <- tribble(~term, ~alc_full, ~alc_full, ~alc_weekend, ~alc_weekend, ~alc_weekday,~alc_weekday,
                         "Mean of Dependent Variable", find_mean(semester_level, alcohol_offense_per25), find_mean(semester_level, alcohol_offense_per25),
                         find_mean(semester_level_weekends, alcohol_offense_per25), find_mean(semester_level_weekends, alcohol_offense_per25),
                         find_mean(semester_level_weekdays, alcohol_offense_per25), find_mean(semester_level_weekdays, alcohol_offense_per25))

add_means_drug <- tribble(~term, ~alc_full, ~alc_full, ~alc_weekend, ~alc_weekend, ~alc_weekday,~alc_weekday,
                          "Mean of Dependent Variable", find_mean(semester_level, drug_offense_per25), find_mean(semester_level, drug_offense_per25),
                          find_mean(semester_level_weekends, drug_offense_per25), find_mean(semester_level_weekends, drug_offense_per25),
                          find_mean(semester_level_weekdays, drug_offense_per25), find_mean(semester_level_weekdays, drug_offense_per25))


add_means_sex <- tribble(~term, ~alc_full, ~alc_full, ~alc_weekend, ~alc_weekend, ~alc_weekday,~alc_weekday,
                         "Mean of Dependent Variable", find_mean(semester_level, sexual_assault_per25), find_mean(semester_level, sexual_assault_per25),
                         find_mean(semester_level_weekends, sexual_assault_per25), find_mean(semester_level_weekends, sexual_assault_per25),
                         find_mean(semester_level_weekdays, sexual_assault_per25), find_mean(semester_level_weekdays, sexual_assault_per25))
attr(add_means_alc, "position") <- c(7)
attr(add_means_drug, "position") <- c(7)
attr(add_means_sex, "position") <- c(7)

# regressions -------------------------------------------------------------

alc_ols <- semester_level %>% 
  feols(alcohol_offense_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator| university + semester_number,
        cluster = ~university, data = .)
alc_ols_weekends <- semester_level_weekends %>% 
  feols(alcohol_offense_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator| university + semester_number,
        cluster = ~university, data = .)
alc_ols_weekdays <- semester_level_weekdays %>% 
  feols(alcohol_offense_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university + semester_number,
        cluster = ~university, data = .)

alc_ols_fe <- semester_level %>% 
  feols(alcohol_offense_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
alc_ols_weekends_fe <- semester_level_weekends %>% 
  feols(alcohol_offense_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
alc_ols_weekdays_fe <- semester_level_weekdays %>% 
  feols(alcohol_offense_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)



drug_ols <- semester_level %>% 
  feols(drug_offense_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university + semester_number,
        cluster = ~university, data = .)
drug_ols_weekends <- semester_level_weekends %>% 
  feols(drug_offense_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator| university + semester_number,
        cluster = ~university, data = .)
drug_ols_weekdays <- semester_level_weekdays %>% 
  feols(drug_offense_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university + semester_number,
        cluster = ~university, data = .)

drug_ols_fe <- semester_level %>% 
  feols(drug_offense_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
drug_ols_weekends_fe <- semester_level_weekends %>% 
  feols(drug_offense_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
drug_ols_weekdays_fe <- semester_level_weekdays %>% 
  feols(drug_offense_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)


sex_ols <- semester_level %>% 
  feols(sexual_assault_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university + semester_number,
        cluster = ~university, data = .)
sex_ols_weekends <- semester_level_weekends %>% 
  feols(sexual_assault_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university + semester_number,
        cluster = ~university, data = .)
sex_ols_weekdays <- semester_level_weekdays %>% 
  feols(drug_offense_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator| university + semester_number,
        cluster = ~university, data = .)

sex_ols_fe <- semester_level %>% 
  feols(sexual_assault_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator| university_by_semester_number + semester_number,
        cluster = ~university, data = .)
sex_ols_weekends_fe <- semester_level_weekends %>% 
  feols(sexual_assault_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
sex_ols_weekdays_fe <- semester_level_weekdays %>% 
  feols(sexual_assault_per25 ~semester_before_dose_indicator + treatment + semester_after_dose_indicator| university_by_semester_number + semester_number,
        cluster = ~university, data = .)


## first list the full sample models
full_sample <- list(alc_ols, alc_ols_fe, drug_ols, drug_ols_fe, sex_ols, sex_ols_fe)
weekend_sample <- list(alc_ols_weekends, alc_ols_weekends_fe,
                       drug_ols_weekends, drug_ols_weekends_fe,
                       sex_ols_weekends, sex_ols_weekends_fe)
weekday_sample <- list(alc_ols_weekdays, alc_ols_weekdays_fe,
                       drug_ols_weekdays, drug_ols_weekdays_fe,
                       sex_ols_weekdays, sex_ols_weekdays_fe)
gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-by-Year", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: year", "FE: Year", ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
              "FE: date", "FE: Day-by-Month-by-Year", ~fmt)
gm_first <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs", ~fmt)

full_sample_part <- modelsummary(full_sample, stars = T, output = "data.frame",
             coef_map = c("semester_before_dose_indicator" = "Semester Before",
                          "treatment" = "Moratorium",
                          "semester_after_dose_indicator" = "Semester After"),
             gof_map = gm_first) %>% 
  mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>% 
  select(matches("term|^model")) 
weekend_sample_part <- modelsummary(weekend_sample, stars = T, output = "data.frame",
             coef_map = c("semester_before_dose_indicator" = "Semester Before",
                          "treatment" = "Moratorium",
                          "semester_after_dose_indicator" = "Semester After",
                          "week_before" = "Week Before",
                          "week_after" = "Week After"),
             gof_map = gm_first) %>% 
  mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>% 
  select(matches("term|^model")) 

weekdays_sample <- modelsummary(weekend_sample, stars = T, output = "data.frame",
             coef_map = c("semester_before_dose_indicator" = "Semester Before",
                          "treatment" = "Moratorium",
                          "semester_after_dose_indicator" = "Semester After"),
             gof_map = gm) %>% 
  mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>% 
  select(matches("term|^model")) 



ifc::main_table(full_sample, weekend_sample, weekday_sample)
