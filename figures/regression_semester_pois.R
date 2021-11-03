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

daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")

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
                ~(./total_enrollment) * 25000, .names = "{.col}_")) %>% 
  mutate(semester_season = ifelse(semester_number %% 2 == 0, 1,2)) %>% 
  group_by(university, semester_season) %>% 
  mutate(university_by_semester_number = cur_group_id()) %>% 
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
                ~(./total_enrollment) * 25000, .names = "{.col}_")) %>% 
  ungroup() %>% 
  mutate(semester_season = ifelse(semester_number %% 2 == 0, 1,2)) %>% 
  group_by(university, semester_season) %>% 
  mutate(university_by_semester_number = cur_group_id()) %>% 
  ungroup()

# 
# TwoWayFEWeights::twowayfeweights(semester_level_weekends,
#                                  "alcohol_offense ",
#                                  "university",
#                                  "semester_number",
#                                  "treatment",
#                                  cmd_type = "feTR") %>% View()


## weekedays 
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



# function for means ------------------------------------------------------

find_mean <- function(data, column) {
  column_mean <- data %>% 
    summarize(mean({{column}}, na.rm = T)) %>% 
    pull()
  return(column_mean)
}

add_means_alc <- tribble(~term, ~alc_full, ~alc_full, ~alc_weekend, ~alc_weekend, ~alc_weekday,~alc_weekday,
                         "Mean of Dependent Variable", find_mean(semester_level, alcohol_offense), find_mean(semester_level, alcohol_offense ),
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
attr(add_means_alc, "position") <- c(3)
attr(add_means_drug, "position") <- c(3)
attr(add_means_sex, "position") <- c(3)

# regressions -------------------------------------------------------------

alc_pois <- semester_level %>% 
  fepois(alcohol_offense  ~treatment | university + semester_number,
        cluster = ~university, data = .)
alc_pois_weekends <- semester_level_weekends %>% 
  fepois(alcohol_offense  ~treatment | university + semester_number,
        cluster = ~university, data = .)
alc_pois_weekdays <- semester_level_weekdays %>% 
  fepois(alcohol_offense  ~treatment | university + semester_number,
        cluster = ~university, data = .)

alc_pois_fe <- semester_level %>% 
  fepois(alcohol_offense  ~treatment | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
alc_pois_weekends_fe <- semester_level_weekends %>% 
  fepois(alcohol_offense  ~treatment | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
alc_pois_weekdays_fe <- semester_level_weekdays %>% 
  fepois(alcohol_offense  ~treatment | university_by_semester_number + semester_number,
        cluster = ~university, data = .)

alc_models <- list("(1)" = alc_pois,
                   "(2)" = alc_pois_fe,
                   "(1)" = alc_pois_weekends,
                   "(2)" = alc_pois_weekends_fe,
                   "(1)" = alc_pois_weekdays,
                   "(2)" = alc_pois_weekdays_fe)


drug_pois <- semester_level %>% 
  fepois(drug_offense  ~treatment | university + semester_number,
        cluster = ~university, data = .)
drug_pois_weekends <- semester_level_weekends %>% 
  fepois(drug_offense  ~treatment | university + semester_number,
        cluster = ~university, data = .)
drug_pois_weekdays <- semester_level_weekdays %>% 
  fepois(drug_offense  ~treatment | university + semester_number,
        cluster = ~university, data = .)

drug_pois_fe <- semester_level %>% 
  fepois(drug_offense  ~treatment | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
drug_pois_weekends_fe <- semester_level_weekends %>% 
  fepois(drug_offense  ~treatment | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
drug_pois_weekdays_fe <- semester_level_weekdays %>% 
  fepois(drug_offense  ~treatment | university_by_semester_number + semester_number,
        cluster = ~university, data = .)

drug_models <- list("(1)" = drug_pois,
                    "(2)" = drug_pois_fe,
                    "(1)" = drug_pois_weekends,
                    "(2)" = drug_pois_weekends_fe,
                    "(1)" = drug_pois_weekdays,
                    "(2)" = drug_pois_weekdays_fe)

sex_pois <- semester_level %>% 
  fepois(sexual_assault  ~treatment | university + semester_number,
        cluster = ~university, data = .)
sex_pois_weekends <- semester_level_weekends %>% 
  fepois(sexual_assault  ~treatment | university + semester_number,
        cluster = ~university, data = .)
sex_pois_weekdays <- semester_level_weekdays %>% 
  fepois(drug_offense  ~treatment | university + semester_number,
        cluster = ~university, data = .)

sex_pois_fe <- semester_level %>% 
  fepois(sexual_assault  ~treatment | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
sex_pois_weekends_fe <- semester_level_weekends %>% 
  fepois(sexual_assault  ~treatment | university_by_semester_number + semester_number,
        cluster = ~university, data = .)
sex_pois_weekdays_fe <- semester_level_weekdays %>% 
  fepois(sexual_assault  ~treatment | university_by_semester_number + semester_number,
        cluster = ~university, data = .)

sex_models <- list("(1)" = sex_pois,
                   "(2)" = sex_pois_fe,
                   "(1)" = sex_pois_weekends,
                   "(2)" = sex_pois_weekends_fe,
                   "(1)" = sex_pois_weekdays,
                   "(2)" = sex_pois_weekdays_fe)

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-by-Year", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: year", "FE: Year", ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
              "FE: date", "FE: Day-by-Month-by-Year", ~fmt)


alc_p <- modelsummary(alc_models, stars = T,
             gof_omit = 'DF|Deviance|AIC|BIC|Log|R2|St',
             coef_map = c("week_before" = "Week Before",
                          "treatment" = "Moratorium",
                          "week_after" = "Week After"),
             title = "\\label{alc_offense_p}Effect of Moratoriums on Alcohol Offenses (Poisson Regression)",
             notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                          "Standard errors are clustered by university.",
                          "Outcome of interest is alcohol offenses counts.",
                          "Coefficient estimates shown are for Moratorium.",
                          "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
             gof_map = gm,
             add_rows = add_means_alc) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 2, "Weekends (Fri-Sat)" = 2, "Weekdays (Mon-Thurs)" = 2))

drug_p <- modelsummary(drug_models, stars = T,
             gof_omit = 'DF|Deviance|AIC|BIC|Log|R2|St',
             coef_map = c("week_before" = "Week Before",
                          "treatment" = "Moratorium",
                          "week_after" = "Week After"),
             title = "\\label{drug_offense_p}Effect of Moratoriums on Drug Offenses (Poisson Regression)",
             notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                          "Standard errors are clustered by university.",
                          "Outcome of interest is drug offenses counts.",
                          "Coefficient estimates shown are for Moratorium.",
                          "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
             gof_map = gm, 
             add_rows = add_means_drug) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 2, "Weekends (Fri-Sat)" = 2, "Weekdays (Mon-Thurs)" = 2))

sex_p <- modelsummary(sex_models, stars = T,
             gof_omit = 'DF|Deviance|AIC|BIC|Log|R2|St',
             coef_map = c("week_before" = "Week Before",
                          "treatment" = "Moratorium",
                          "week_after" = "Week After"),
             title = "\\label{sex_offense_p}Effect of Moratoriums on Sexual Assaults (Poisson Regression)",
             notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                          "Standard errors are clustered by university.",
                          "Outcome of interest is sexual assaults counts.",
                          "Coefficient estimates shown are for Moratorium.",
                          "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
             gof_map = gm,
             add_rows = add_means_sex) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 2, "Weekends (Fri-Sat)" = 2, "Weekdays (Mon-Thurs)" = 2))
