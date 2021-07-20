## Purpose of script: predicting if moratorium occurs or not in all universities
## i only used pre-treatment data for this. I also standardized the variables before estimatation
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-10
##

library(tidyverse)
library(readxl)
library(fixest)
library(kableExtra)
library(modelsummary)

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv")

   

ipeds <- read_csv("Created Data/IPEDS/ipeds_final.csv")

uni_covariates <- colnames(ipeds)
uni_covariates <- uni_covariates[!(uni_covariates %in% c("unit_id", "university", "year",
                                                     "control_of_institution", "calendar_system",
                                                     "state_of_institution", "level_of_institution",
                                                      "state_abbreviation"))]
ipeds_subset <- ipeds %>% 
  select(c("control_of_institution", "calendar_system",
           "level_of_institution", "university")) %>% 
  distinct() %>% 
  filter(university %in% daily_crime$university)

## collapsing to means - I only use data pre-first closure
prediction_data <- daily_crime %>% 
  filter(date < closure_1 | university %in% ifc::untreated_universities()) %>% 
  group_by(university) %>% 
  summarize(across(all_of(uni_covariates), ~mean(., na.rm = T))) %>% 
  mutate(ever_moratorium = ifelse(!(university %in% ifc::untreated_universities()), 1, 0 ))  %>% 
  left_join(ipeds_subset, by = c("university" = "university")) %>% 
  fastDummies::dummy_cols(select_columns = c("control_of_institution", "calendar_system", "level_of_institution"))


ols_predict_death <- prediction_data %>% 
  mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(missing = ifelse(if_any(c(sat_math_25, sat_reading_75,
                                   frac_core_revenues_priv_gifts_grants,
                                   frac_core_revenues_tuition_fees), ~. == 0), 1, 0)) %>% 
  mutate(across(where(is.double), ~scale(., center = T))) %>% 
  mutate(ever_moratorium = ifelse(!(university %in% ifc::untreated_universities()), 1, 0 )) %>% 
  feols(ever_moratorium ~ total_price_oncampus_oos  + total_price_oncampus_is +
          undergraduate_enrollment +
          frac_admitted_total +
          frac_undergrad_black + frac_undergrad_asian +
          frac_undergrad_hispanic_latino +
          frac_undergrad_white +
          frac_ftime_undergrad_foreign_countries +
          graduation_rate_total_cohort +
          student_to_faculty_ratio +
          sat_math_75 + sat_reading_75 +
          frac_core_revenues_tuition_fees +
          frac_core_revenues_priv_gifts_grants +
          missing, cluster = ~university, data = .) 

logit_predict_death <- prediction_data %>% 
  mutate(across(everything(), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(missing = ifelse(if_any(c(sat_math_25, sat_reading_75,
                                   frac_core_revenues_priv_gifts_grants,
                                   frac_core_revenues_tuition_fees), ~. == 0), 1, 0)) %>% 
  mutate(across(where(is.double), ~scale(., center = T))) %>% 
  mutate(ever_moratorium = ifelse(!(university %in% ifc::untreated_universities()), 1, 0 )) %>% 
  femlm(ever_moratorium ~ total_price_oncampus_oos  + total_price_oncampus_is +
          undergraduate_enrollment +
          frac_admitted_total +
          frac_undergrad_black + frac_undergrad_asian +
          frac_undergrad_hispanic_latino +
          frac_undergrad_white +
          frac_ftime_undergrad_foreign_countries +
          graduation_rate_total_cohort +
          student_to_faculty_ratio +
          sat_math_75 + sat_reading_75 +
          frac_core_revenues_tuition_fees +
          frac_core_revenues_priv_gifts_grants +
          missing, family = "logit", cluster = ~university, data = .)


predict_list <- list("OLS" = ols_predict_death, "Logit" = logit_predict_death)

ols_predict_death_tidy <- broom::tidy(ols_predict_death, conf.int = T) %>% 
  mutate(estimation = "OLS")

logit_predict_death_tidy <- broom::tidy(logit_predict_death, conf.int = T)

prediction_table <- modelsummary(
  predict_list,
  stars = T,
  statistic = c("s.e. = {std.error}",
                "p = {p.value}"),
  gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
  coef_omit = "Intercept",
  coef_rename = c(
    "total_price_oncampus_oos" = "Total Cost out-of-state (on campus)" ,
    "total_price_oncampus_is" = "Total Cost in-state (on campus)" ,
    "undergraduate_enrollment" = "Undergraduate Enrollment",
    "frac_admitted_total" = "Fraction Admitted",
    "frac_undergrad_black" =  "Fraction Undergrad Black" ,
    "frac_undergrad_asian" = "Fraction Undergrad Asian" ,
    "frac_undergrad_white" = "Fraction Undergrad White" ,
    "frac_undergrad_hispanic_latino" = "Fraction Hispanic",
    "frac_ftime_undergrad_foreign_countries" = "Fraction Undergrad Foreign" ,
    "graduation_rate_total_cohort" = "Graduation Rate" ,
    "student_to_faculty_ratio" = "Student-to-faculty Ratio",
    "sat_math_75" = "SAT Math 75th Percentile",
    "sat_reading_75" = "SAT Reading 75th Percentile",
    "frac_core_revenues_tuition_fees" = "Fraction Revenues from Tuition Fees",
    "frac_core_revenues_priv_gifts_grants" = "Fraction Revenues from Private Gifts/Grants",
    "missing" = "Missing Data"
  ),
  title = "OLS and Logit estimates predicting fraternity moratoria. Outcome variable is an indicator for ever having a moratorium.
  Estimates are based on prior-to-first-moratorium data."
)


# graphs ------------------------------------------------------------------




ols_prediction_graph  <- ols_predict_death %>% 
  modelplot(coef_omit = "Inter",coef_rename = c(
    "total_price_oncampus_oos" = "Total Cost out-of-state (on campus)" ,
    "total_price_oncampus_is" = "Total Cost in-state (on campus)" ,
    "undergraduate_enrollment" = "Undergraduate Enrollment",
    "frac_admitted_total" = "Fraction Admitted",
    "frac_undergrad_black" =  "Fraction Undergrad Black" ,
    "frac_undergrad_asian" = "Fraction Undergrad Asian" ,
    "frac_undergrad_white" = "Fraction Undergrad White" ,
    "frac_undergrad_hispanic_latino" = "Fraction Hispanic",
    "frac_ftime_undergrad_foreign_countries" = "Fraction Undergrad Foreign" ,
    "graduation_rate_total_cohort" = "Graduation Rate" ,
    "student_to_faculty_ratio" = "Student-to-faculty Ratio",
    "sat_math_75" = "SAT Math 75th Percentile",
    "sat_reading_75" = "SAT Reading 75th Percentile",
    "frac_core_revenues_tuition_fees" = "Fraction Revenues from Tuition Fees",
    "frac_core_revenues_priv_gifts_grants" = "Fraction Revenues from Private Gifts/Grants",
    "missing" = "Missing Data"
  )) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(x = "")


logit_predict_graph <- logit_predict_death %>% 
  modelplot(coef_omit = "Inter",coef_rename = c(
    "total_price_oncampus_oos" = "Total Cost out-of-state (on campus)" ,
    "total_price_oncampus_is" = "Total Cost in-state (on campus)" ,
    "undergraduate_enrollment" = "Undergraduate Enrollment",
    "frac_admitted_total" = "Fraction Admitted",
    "frac_undergrad_black" =  "Fraction Undergrad Black" ,
    "frac_undergrad_asian" = "Fraction Undergrad Asian" ,
    "frac_undergrad_white" = "Fraction Undergrad White" ,
    "frac_undergrad_hispanic_latino" = "Fraction Hispanic",
    "frac_ftime_undergrad_foreign_countries" = "Fraction Undergrad Foreign" ,
    "graduation_rate_total_cohort" = "Graduation Rate" ,
    "student_to_faculty_ratio" = "Student-to-faculty Ratio",
    "sat_math_75" = "SAT Math 75th Percentile",
    "sat_reading_75" = "SAT Reading 75th Percentile",
    "frac_core_revenues_tuition_fees" = "Fraction Revenues from Tuition Fees",
    "frac_core_revenues_priv_gifts_grants" = "Fraction Revenues from Private Gifts/Grants",
    "missing" = "Missing Data"
  )) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(x = "")

