## Purpose of script: predicting if moratorium occurs or not in death universities
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-10
##

library(tidyverse)
library(readxl)
library(fixest)
library(modelsummary)

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv",
                        guess_max = 50000)

death_universities <- daily_crime %>% 
 filter(university %in% ifc::death_universities()) 
   

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
  filter(university %in% ifc::death_universities())

## collapsing to means
death_universities <- death_universities %>% 
  group_by(university) %>% 
  summarize(across(uni_covariates, ~mean(., na.rm = T))) %>% 
  mutate(ever_moratorium = ifelse(!(university %in% ifc::untreated_universities()), 1, 0 ))  %>% 
  left_join(ipeds_subset, by = c("university" = "university")) %>% 
  fastDummies::dummy_cols(select_columns = c("control_of_institution", "calendar_system", "level_of_institution"))


ols_predict_death <- death_universities %>% 
  feols(ever_moratorium ~ total_price_oncampus_oos  + total_price_oncampus_is +
          undergraduate_enrollment +
          frac_admitted_total +
          frac_undergrad_black + frac_undergrad_asian +
          frac_undergrad_hispanic_latino +
          frac_total_white +
          frac_ftime_undergrad_foreign_countries +
          graduation_rate_total_cohort +
          student_to_faculty_ratio , cluster = ~university, data = .) 

logit_predict_death <- death_universities %>% 
  femlm(ever_moratorium ~ total_price_oncampus_oos  + total_price_oncampus_is +
          undergraduate_enrollment +
          frac_admitted_total +
          frac_undergrad_black + frac_undergrad_asian +
          frac_undergrad_hispanic_latino +
          frac_total_white +
          frac_ftime_undergrad_foreign_countries +
          graduation_rate_total_cohort +
          student_to_faculty_ratio 
          , family = "logit", cluster = ~university, data = .)

ols_predict_death_tidy <- broom::tidy(ols_predict_death, conf.int = T) %>% 
  mutate(estimation = "OLS")

logit_predict_death_tidy <- broom::tidy(logit_predict_death, conf.int = T)
logit_predict_death_tidy %>% 
  mutate(estimation = "Logit") %>% 
  bind_rows(ols_predict_death_tidy) %>% 
  mutate(term = case_when(
    term == "total_price_oncampus_oos" ~ "Total Cost out-of-state (on campus)",
    term == "total_price_oncampus_is" ~ "Total Cost in-state (on campus)",
    term == "undergraduate_enrollment" ~ "Undergraduate Enrollment",
    term == "frac_admitted_total" ~ "Fraction Admitted",
    term == "frac_undergrad_black" ~ "Fraction Undergrad Black",
    term == "frac_undergrad_asian" ~ "Fraction Undergrad Asian",
    term == "frac_undergrad_white" ~ "Fraction Undergrad White",
    term == "frac_ftime_undergrad_foreign_countries" ~ "Fraction Undergrad Foreign",
    term == "graduate_rate_total_cohort" ~ "Graduation Rate",
    term == "student_to_faculty_ratio" ~ "Student-to-faculty Ratio",
    term == "(Intercept)" ~ "intercept"
  )) %>% 
  filter(term != "intercept") %>% 
  group_by(estimation) %>% 
  mutate(row_number = row_number()) %>% 
  ggplot(aes(row_number, p.value, color = estimation)) +
  geom_point() +
  ylim(0, 1) +
  xlim(-1,10) +
  geom_hline(aes(yintercept = 0.05), color = "red", linetype = "dashed") +
  geom_text(aes(row_number, p.value, label = term), nudge_y = 0.015, size = 2.5) +
  scale_color_manual(values=c("#999999", "#56B4E9")) +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(color = "", x = "", y = "")
death_universities %>% 
  femlm(ever_moratorium ~ frac_core_revenues_tuition_fees +
          frac_core_revenues_priv_gifts_grants + undergraduate_enrollment +
          graduation_rate_total_cohort + frac_undergrad_asian +
          frac_undergrad_black + frac_undergrad_white + 
          total_price_oncampus_oos + total_price_oncampus_is +
          + frac_admitted_total + student_to_faculty_ratio
        , family = "logit", cluster = ~university, data = .) %>% 
  summary()










daily_crime_collapse <- daily_crime %>% 
  group_by(university) %>% 
  summarize(across(uni_covariates, ~mean(., na.rm = T))) %>% 
  mutate(ever_moratorium = ifelse(!(university %in% ifc::untreated_universities()), 1, 0 ))  %>% 
  left_join(ipeds_subset, by = c("university" = "university")) %>% 
  fastDummies::dummy_cols(select_columns = c("control_of_institution", "calendar_system", "level_of_institution"))
daily_crime_collapse %>% 
  femlm(ever_moratorium ~ total_price_oncampus_oos  + total_price_oncampus_is +
          undergraduate_enrollment +
          frac_admitted_total +
          frac_undergrad_black + frac_undergrad_asian +
          frac_undergrad_hispanic_latino +
          frac_total_white +
          frac_ftime_undergrad_foreign_countries +
          graduation_rate_total_cohort +
          student_to_faculty_ratio  + frac_core_revenues_priv_gifts_grants +
          frac_core_revenues_tuition_fees + fulltime_retention_rate ,
          family = "logit", cluster = ~university, data = .) %>% 
  summary()
daily_crime_collapse %>% 
  feols(ever_moratorium ~ total_price_oncampus_oos  + total_price_oncampus_is +
          undergraduate_enrollment +
          frac_admitted_total +
          frac_undergrad_black + frac_undergrad_asian +
          frac_undergrad_hispanic_latino +
          frac_total_white +
          frac_ftime_undergrad_foreign_countries +
          graduation_rate_total_cohort +
          student_to_faculty_ratio  + frac_core_revenues_priv_gifts_grants +
          frac_core_revenues_tuition_fees + fulltime_retention_rate ,
        family = "logit", cluster = ~university, data = .) %>% 
  summary()
