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
                         "Mean of Dependent Variable", find_mean(semester_level, alcohol_offense ), find_mean(semester_level, alcohol_offense ),
                         find_mean(semester_level_weekends, alcohol_offense ), find_mean(semester_level_weekends, alcohol_offense ),
                         find_mean(semester_level_weekdays, alcohol_offense ), find_mean(semester_level_weekdays, alcohol_offense ))

add_means_drug <- tribble(~term, ~alc_full, ~alc_full, ~alc_weekend, ~alc_weekend, ~alc_weekday,~alc_weekday,
                          "Mean of Dependent Variable", find_mean(semester_level, drug_offense ), find_mean(semester_level, drug_offense ),
                          find_mean(semester_level_weekends, drug_offense ), find_mean(semester_level_weekends, drug_offense ),
                          find_mean(semester_level_weekdays, drug_offense ), find_mean(semester_level_weekdays, drug_offense ))


add_means_sex <- tribble(~term, ~alc_full, ~alc_full, ~alc_weekend, ~alc_weekend, ~alc_weekday,~alc_weekday,
                         "Mean of Dependent Variable", find_mean(semester_level, sexual_assault ), find_mean(semester_level, sexual_assault ),
                         find_mean(semester_level_weekends, sexual_assault ), find_mean(semester_level_weekends, sexual_assault ),
                         find_mean(semester_level_weekdays, sexual_assault ), find_mean(semester_level_weekdays, sexual_assault ))
attr(add_means_alc, "position") <- c(7)
attr(add_means_drug, "position") <- c(7)
attr(add_means_sex, "position") <- c(7)

# regressions -------------------------------------------------------------

alc_ols_p <- semester_level %>% 
  fepois(alcohol_offense  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator| university + semester_number,
        cluster = ~university, data = .)
alc_ols_weekends_p <- semester_level_weekends %>% 
  fepois(alcohol_offense  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator| university + semester_number,
        cluster = ~university, data = .)
alc_ols_weekdays_p <- semester_level_weekdays %>% 
  fepois(alcohol_offense  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university + semester_number,
        cluster = ~university, data = .)

alc_ols_fe_p <- semester_level %>% 
  fepois(alcohol_offense  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
alc_ols_weekends_fe_p <- semester_level_weekends %>% 
  fepois(alcohol_offense  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
alc_ols_weekdays_fe_p <- semester_level_weekdays %>% 
  fepois(alcohol_offense  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)

alc_models_p <- list("(1)" = alc_ols_p,
                   "(2)" = alc_ols_fe_p,
                   "(1)" = alc_ols_weekends_p,
                   "(2)" = alc_ols_weekends_fe_p,
                   "(1)" = alc_ols_weekdays_p,
                   "(2)" = alc_ols_weekdays_fe_p)


drug_ols_p <- semester_level %>% 
  fepois(drug_offense  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university + semester_number,
        cluster = ~university, data = .)
drug_ols_weekends_p <- semester_level_weekends %>% 
  fepois(drug_offense  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator| university + semester_number,
        cluster = ~university, data = .)
drug_ols_weekdays_p <- semester_level_weekdays %>% 
  fepois(drug_offense  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university + semester_number,
        cluster = ~university, data = .)

drug_ols_fe_p <- semester_level %>% 
  fepois(drug_offense  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
drug_ols_weekends_fe_p <- semester_level_weekends %>% 
  fepois(drug_offense  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
drug_ols_weekdays_fe_p <- semester_level_weekdays %>% 
  fepois(drug_offense  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)

drug_models_p <- list("(1)" = drug_ols_p,
                    "(2)" = drug_ols_fe_p,
                    "(1)" = drug_ols_weekends_p,
                    "(2)" = drug_ols_weekends_fe_p,
                    "(1)" = drug_ols_weekdays_p,
                    "(2)" = drug_ols_weekdays_fe_p)

sex_ols_p <- semester_level %>% 
  fepois(sexual_assault  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university + semester_number,
        cluster = ~university, data = .)
sex_ols_weekends_p <- semester_level_weekends %>% 
  fepois(sexual_assault  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university + semester_number,
        cluster = ~university, data = .)
sex_ols_weekdays_p <- semester_level_weekdays %>% 
  fepois(drug_offense  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator| university + semester_number,
        cluster = ~university, data = .)

sex_ols_fe_p <- semester_level %>% 
  fepois(sexual_assault  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator| university_by_semester_number + semester_number,
        cluster = ~university, data = .)
sex_ols_weekends_fe_p <- semester_level_weekends %>% 
  fepois(sexual_assault  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
sex_ols_weekdays_fe_p <- semester_level_weekdays %>% 
  fepois(sexual_assault  ~semester_before_dose_indicator + treatment + semester_after_dose_indicator| university_by_semester_number + semester_number,
        cluster = ~university, data = .)

sex_models_p <- list("(1)" = sex_ols_p,
                   "(2)" = sex_ols_fe_p,
                   "(1)" = sex_ols_weekends_p,
                   "(2)" = sex_ols_weekends_fe_p,
                   "(1)" = sex_ols_weekdays_p,
                   "(2)" = sex_ols_weekdays_fe_p)

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-by-Year", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: year", "FE: Year", ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
              "FE: date", "FE: Day-by-Month-by-Year", ~fmt)


alc_p <- modelsummary(alc_models_p, stars = T,
                    gof_omit = 'DF|Deviance|AIC|BIC|Log|R2|St',
                    coef_map = c("semester_before_dose_indicator" = "Semester Before",
                                 "treatment" = "Moratorium",
                                 "semester_after_dose_indicator" = "Semester After"),
                    title = "\\label{alc_offense_p}Effect of Moratoriums on Alcohol Offenses (Poisson)",
                    notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                                 "Standard errors are clustered by university.",
                                 "Outcome of interest is alcohol offense counts.",
                                 "Coefficient estimates shown are for Moratorium.",
                                 "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
                    gof_map = gm,
                    add_rows = add_means_alc) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 2, "Weekends (Fri-Sat)" = 2, "Weekdays (Mon-Thurs)" = 2))

drug_p <- modelsummary(drug_models_p, stars = T,
                     gof_omit = 'DF|Deviance|AIC|BIC|Log|R2|St',
                     coef_map = c("semester_before_dose_indicator" = "Semester Before",
                                  "treatment" = "Moratorium",
                                  "semester_after_dose_indicator" = "Semester After"),
                     title = "\\label{drug_offense_p}Effect of Moratoriums on Drug Offenses (Poisson)",
                     notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                                  "Standard errors are clustered by university.",
                                  "Outcome of interest is drug offense counts.",
                                  "Coefficient estimates shown are for Moratorium.",
                                  "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
                     gof_map = gm, 
                     add_rows = add_means_drug) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 2, "Weekends (Fri-Sat)" = 2, "Weekdays (Mon-Thurs)" = 2))

sex_p <- modelsummary(sex_models_p, stars = T,
                    gof_omit = 'DF|Deviance|AIC|BIC|Log|R2|St',
                    coef_map = c("semester_before_dose_indicator" = "Semester Before",
                                 "treatment" = "Moratorium",
                                 "semester_after_dose_indicator" = "Semester After"),
                    title = "\\label{sex_offense_p}Effect of Moratoriums on Sexual Assaults (Poisson)",
                    notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                                 "Standard errors are clustered by university.",
                                 "Outcome of interest is sexual assault counts.",
                                 "Coefficient estimates shown are for Moratorium.",
                                 "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
                    gof_map = gm,
                    add_rows = add_means_sex) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 2, "Weekends (Fri-Sat)" = 2, "Weekdays (Mon-Thurs)" = 2))
