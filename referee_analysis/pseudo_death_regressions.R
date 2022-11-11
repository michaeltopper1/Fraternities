## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-10-21
##

library(tidyverse)
library(lubridate)
library(fixest)
library(panelsummary)

laag <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + dplyr::lag(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}

daily_crime_nevertreated <- read_csv("created_data/xmaster_data/daily_panel_allschools.csv") 

death_dates <-read_csv("data/death_never_treated_dates.csv") %>% 
    mutate(death_date = mdy(death_date))

daily_crime_nevertreated_pseudo <- daily_crime_nevertreated %>% 
  filter(university %in% ifc::death_untreated_universities()) 


daily_crime_nevertreated_pseudo  <- daily_crime_nevertreated_pseudo %>% 
  left_join(death_dates, by = "university") 


# creating the pseudo treatment with 64 days ------------------------------

daily_crime_nevertreated_pseudo <- daily_crime_nevertreated_pseudo %>% 
  select(-treatment) %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  relocate(date, death_date) %>% 
  mutate(treatment = ifelse(date == death_date, 1, 0), .before = 1) %>% 
  mutate(treatment = laag(treatment, c(0:63))) 

daily_crime_nevertreated_pseudo_weekends <- daily_crime_nevertreated_pseudo %>% 
  filter(day_of_week == "Sat" | 
           day_of_week == "Sun" |
           day_of_week == "Fri")
daily_crime_nevertreated_pseudo_weekdays <- daily_crime_nevertreated_pseudo %>% 
  filter(!(day_of_week == "Sat" | 
           day_of_week == "Sun" |
           day_of_week == "Fri"))

explanatory_vars <- c("treatment")

fixed_effects_2 <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")


alc_pseudo <- ifc::reghdfe(daily_crime_nevertreated_pseudo, c("alcohol_offense_per25"),
                           explanatory_vars, fixed_effects_2, "university")

alc_pseudo_weekends <- ifc::reghdfe(daily_crime_nevertreated_pseudo_weekends, c("alcohol_offense_per25"),
             explanatory_vars, fixed_effects_2, "university")

alc_pseudo_weekdays <- ifc::reghdfe(daily_crime_nevertreated_pseudo_weekdays, c("alcohol_offense_per25"),
             explanatory_vars, fixed_effects_2, "university")


sex_pseudo <- ifc::reghdfe(daily_crime_nevertreated_pseudo, 
                           c("sexual_assault_per25"),
                           explanatory_vars, 
                           fixed_effects_2, 
                           "university")
sex_pseudo_weekends <- ifc::reghdfe(daily_crime_nevertreated_pseudo_weekends, 
             c("sexual_assault_per25"),
             explanatory_vars, 
             fixed_effects_2, 
             "university")

sex_pseudo_weekdays <- ifc::reghdfe(daily_crime_nevertreated_pseudo_weekdays, 
             c("sexual_assault_per25"),
             explanatory_vars, 
             fixed_effects_2, 
             "university")
gof_mapping <- ifc::gof_mapping() %>% 
  select(-fmt) %>% 
  mutate(fmt = ifelse(raw == "nobs", 0, 3)) %>% 
  add_row(raw = "mean", clean = "Mean of Dependent Variable", fmt = 3, .after = 1)


pseudo_death <- panelsummary(list(alc_pseudo, alc_pseudo_weekdays, alc_pseudo_weekends),
             list(sex_pseudo, sex_pseudo_weekdays, sex_pseudo_weekends),
             collapse_fe = T, stars = T, 
             panel_labels = c("Panel A: Alcohol Offenses", "Panel B: Sexual Assaults"),
             coef_map = c("treatment" = "64-Day Death Period"),
             gof_map = gof_mapping,
             caption = "\\label{pseudo_death}The Effect of a Fraternity Death on Never-treated Universities",
             mean_dependent = T,
             italic = T,
             bold = F) %>% 
  add_header_above(c(" " = 1, "All Days" = 1, "Weekends" = 1, "Weekdays" = 1)) %>% 
  kableExtra::  footnote(list("Estimates are obtained using OLS. The dependent variable is measured as counts per-25000 enrolled students. Standard errors are clustered that the university leve. The 64-Day Death Period treatment variable is an indicator equal to 1 when there is a fraternity-related death in a university, but no moratorium. This analysis is exclusively for the 15 universities that underwent a fraternity-related death, but did not undergo a fraternity moratorium.",
                              "* p < 0.1, ** p < 0.05, *** p < 0.01"), threeparttable = T)
