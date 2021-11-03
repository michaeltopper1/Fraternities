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

daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv") %>% 
  mutate(sexual_assault_school = ifelse(reason1 %in% c("sexual assault") | reason2 %in% c("sexual assault"), 1,0)) %>%
  filter(sexual_assault_school != 0)

if (!exists("daily_crime_weekdays")) {
  daily_crime_weekdays <- daily_crime %>% 
    filter(day_of_week == "Mon" | day_of_week == "Thu" | day_of_week == "Wed" | day_of_week == "Tue" )
}

if (!exists("daily_crime_weekends")) {
  daily_crime_weekends <- daily_crime %>%
    filter(day_of_week == 'Fri' | day_of_week == "Sat" | day_of_week == "Sun")
}

treatment_daily_crime <- daily_crime %>% 
  group_by(semester_number, university, year) %>% 
  summarize(treatment = mean(treatment))
semester_level <- daily_crime %>% 
  group_by(semester_number, university, year, .drop = F) %>% 
  summarize(across(c(sexual_assault, alcohol_offense, drug_offense, robbery_burglary), ~sum(.,na.rm = T)))


ipeds <- read_csv("created_data/ipeds/ipeds_final.csv")
semester_level <- semester_level %>% 
  left_join(treatment_daily_crime) %>% 
  left_join(ipeds) %>% 
  ungroup() %>% 
  mutate(across(c(sexual_assault, alcohol_offense, drug_offense, robbery_burglary),
                ~(./total_enrollment) * 25000, .names = "{.col}_per25")) %>% 
  mutate(semester_season = ifelse(semester_number %% 2 == 0, 1,2)) %>% 
  group_by(university, semester_season) %>% 
  mutate(university_by_semester_number = cur_group_id()) %>% 
  ungroup()

semester_level <- semester_level %>% 
  group_by(university) %>% 
  mutate(semester_before_dose = lead(treatment)) %>% 
  mutate(semester_after_dose = lag(treatment)) %>% 
  mutate(semester_before_dose = ifelse(semester_before_dose > 0 & treatment >0, 0, semester_before_dose),
         semester_after_dose = ifelse(semester_after_dose > 0 & treatment > 0, 0, semester_after_dose)) %>% 
  mutate(across(c(semester_before_dose, semester_after_dose), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(across(c(semester_before_dose, semester_after_dose), ~ifelse(. >0, 1, 0), .names = "{.col}_indicator")) %>% 
  relocate(semester_number, university, treatment, semester_before_dose, semester_after_dose) %>% 
  ungroup()



# weekends ----------------------------------------------------------------


treatment_daily_crime_weekends <- daily_crime_weekends %>% 
  group_by(semester_number, university, year) %>% 
  summarize(treatment = mean(treatment))
semester_level_weekends <- daily_crime_weekends %>% 
  group_by(semester_number, university, year, .drop = F) %>% 
  summarize(across(c(sexual_assault, alcohol_offense, drug_offense, robbery_burglary), ~sum(.,na.rm = T)))


semester_level_weekends <- semester_level_weekends %>% 
  left_join(treatment_daily_crime) %>% 
  left_join(ipeds) %>% 
  mutate(across(c(sexual_assault, alcohol_offense, drug_offense, robbery_burglary),
                ~(./total_enrollment) * 25000, .names = "{.col}_per25")) %>% 
  ungroup() %>% 
  mutate(semester_season = ifelse(semester_number %% 2 == 0, 1,2)) %>% 
  group_by(university, semester_season) %>% 
  mutate(university_by_semester_number = cur_group_id()) %>% 
  ungroup()

## indicators  before and after
semester_level_weekends <- semester_level_weekends %>% 
  group_by(university) %>% 
  mutate(semester_before_dose = lead(treatment)) %>% 
  mutate(semester_after_dose = lag(treatment)) %>% 
  mutate(semester_before_dose = ifelse(semester_before_dose > 0 & treatment >0, 0, semester_before_dose),
         semester_after_dose = ifelse(semester_after_dose > 0 & treatment > 0, 0, semester_after_dose)) %>% 
  mutate(across(c(semester_before_dose, semester_after_dose), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(across(c(semester_before_dose, semester_after_dose), ~ifelse(. >0, 1, 0), .names = "{.col}_indicator")) %>% 
  relocate(semester_number, university, treatment, semester_before_dose, semester_after_dose) %>% 
  ungroup()


# TwoWayFEWeights::twowayfeweights(semester_level_weekends,
#                                  "alcohol_offense_per25",
#                                  "university_by_semester_number",
#                                  "semester_number",
#                                  "treatment",
#                                  cmd_type = "feTR")


## weekdays 
treatment_daily_crime_weekdays <- daily_crime_weekdays %>% 
  group_by(semester_number, university, year) %>% 
  summarize(treatment = mean(treatment))
semester_level_weekdays <- daily_crime_weekdays %>% 
  group_by(semester_number, university, year, .drop = F) %>% 
  summarize(across(c(sexual_assault, alcohol_offense, drug_offense, robbery_burglary), ~sum(.,na.rm = T)))

semester_level_weekdays <- semester_level_weekdays %>% 
  left_join(treatment_daily_crime) %>% 
  left_join(ipeds) %>% 
  mutate(across(c(sexual_assault, alcohol_offense, drug_offense, robbery_burglary),
                ~(./total_enrollment) * 25000, .names = "{.col}_per25")) %>% 
  ungroup() %>% 
  mutate(semester_season = ifelse(semester_number %% 2 == 0, 1,2)) %>% 
  group_by(university, semester_season) %>% 
  mutate(university_by_semester_number = cur_group_id()) %>% 
  ungroup()

## indicators before and after moratorium
semester_level_weekdays <- semester_level_weekdays %>% 
  group_by(university) %>% 
  mutate(semester_before_dose = lead(treatment)) %>% 
  mutate(semester_after_dose = lag(treatment)) %>% 
  mutate(semester_before_dose = ifelse(semester_before_dose > 0 & treatment >0, 0, semester_before_dose),
         semester_after_dose = ifelse(semester_after_dose > 0 & treatment > 0, 0, semester_after_dose)) %>% 
  mutate(across(c(semester_before_dose, semester_after_dose), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(across(c(semester_before_dose, semester_after_dose), ~ifelse(. >0, 1, 0), .names = "{.col}_indicator")) %>% 
  relocate(semester_number, university, treatment, semester_before_dose, semester_after_dose) %>% 
  ungroup()

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

alc_models <- list("(1)" = alc_ols,
                   "(2)" = alc_ols_fe,
                   "(1)" = alc_ols_weekends,
                   "(2)" = alc_ols_weekends_fe,
                   "(1)" = alc_ols_weekdays,
                   "(2)" = alc_ols_weekdays_fe)


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

drug_models <- list("(1)" = drug_ols,
                    "(2)" = drug_ols_fe,
                    "(1)" = drug_ols_weekends,
                    "(2)" = drug_ols_weekends_fe,
                    "(1)" = drug_ols_weekdays,
                    "(2)" = drug_ols_weekdays_fe)

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

sex_models <- list("(1)" = sex_ols,
                   "(2)" = sex_ols_fe,
                   "(1)" = sex_ols_weekends,
                   "(2)" = sex_ols_weekends_fe,
                   "(1)" = sex_ols_weekdays,
                   "(2)" = sex_ols_weekdays_fe)

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-by-Year", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: year", "FE: Year", ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
              "FE: date", "FE: Day-by-Month-by-Year", ~fmt)


alc <- modelsummary(alc_models, stars = T,
                    gof_omit = 'DF|Deviance|AIC|BIC|Log|R2|St',
                    coef_map = c("semester_before_dose_indicator" = "Semester Before",
                                 "treatment" = "Moratorium",
                                 "semester_after_dose_indicator" = "Semester After"),
                    title = "\\label{alc_offense}Effect of Moratoriums on Alcohol Offenses",
                    notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                                 "Standard errors are clustered by university.",
                                 "Outcome of interest is alcohol offenses per 25 thousand enrolled students.",
                                 "Coefficient estimates shown are for Moratorium.",
                                 "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
                    gof_map = gm,
                    add_rows = add_means_alc) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 2, "Weekends (Fri-Sat)" = 2, "Weekdays (Mon-Thurs)" = 2))

drug <- modelsummary(drug_models, stars = T,
                     gof_omit = 'DF|Deviance|AIC|BIC|Log|R2|St',
                     coef_map = c("semester_before_dose_indicator" = "Semester Before",
                                  "treatment" = "Moratorium",
                                  "semester_after_dose_indicator" = "Semester After"),
                     title = "\\label{drug_offense}Effect of Moratoriums on Drug Offenses",
                     notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                                  "Standard errors are clustered by university.",
                                  "Outcome of interest is drug offenses per 25 thousand enrolled students.",
                                  "Coefficient estimates shown are for Moratorium.",
                                  "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
                     gof_map = gm, 
                     add_rows = add_means_drug) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 2, "Weekends (Fri-Sat)" = 2, "Weekdays (Mon-Thurs)" = 2))

sex <- modelsummary(sex_models, stars = T,
                    gof_omit = 'DF|Deviance|AIC|BIC|Log|R2|St',
                    coef_map = c("semester_before_dose_indicator" = "Semester Before",
                                 "treatment" = "Moratorium",
                                 "semester_after_dose_indicator" = "Semester After"),
                    title = "\\label{sex_offense}Effect of Moratoriums on Sexual Assaults (sexual assault schools only)",
                    notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                                 "Standard errors are clustered by university.",
                                 "Outcome of interest is sexual assaults per 25 thousand enrolled students.",
                                 "Coefficient estimates shown are for Moratorium.",
                                 "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
                    gof_map = gm,
                    add_rows = add_means_sex) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 2, "Weekends (Fri-Sat)" = 2, "Weekdays (Mon-Thurs)" = 2))
