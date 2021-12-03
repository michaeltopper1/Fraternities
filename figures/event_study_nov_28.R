## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-11-28
##

library(tidyverse)
library(fixest)

daily_crime <- read_csv('created_data/xmaster_data/daily_panel.csv')

leead <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + lead(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}

laag <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + dplyr::lag(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}

es <- daily_crime %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(first_day_moratorium = case_when(
    closure_1 == date ~ 1,
    closure_2 == date ~ 1,
    closure_3 == date ~ 1,
    university == "Florida International University" & date == "2018-01-04" ~ 1,
    TRUE ~as.double(0))) %>% 
  mutate(last_day_moratorium = case_when(
    closure_1_end == date + lubridate::days(1) ~ 1,
    closure_2_end == date +lubridate::days(1)~ 1,
    closure_3_end == date + lubridate::days(1)~1,
    university == "San Diego State University" & date == "2014-12-16" ~1,
    university == "University of Virginia-Main Campus" & date == "2014-12-18" ~ 1,
    university == "Washington State University" & date == "2016-12-22" ~1,
    university == "Monmouth University" & date == "2018-12-23" ~1,
    TRUE ~ as.double(0)
  ))


leads <- 4
lags <- 4
bin_length <- 48

starts <- seq(from = 1, to = leads*bin_length, by = bin_length)

lead_counts <- seq(from = bin_length, to = leads *bin_length, by = bin_length)
lag_counts <- seq(from = bin_length, to = lags*bin_length, by = bin_length)

for (i in 1:leads){
  name <- paste0("beta_lead_", i)
  es <- es %>% 
    mutate(!!sym(name) := leead(first_day_moratorium, c(starts[i]:lead_counts[i])))

}


for (i in 1:lags) {
  name <- paste0("beta_lag_", i)
  es <- es %>% 
    mutate(!!sym(name) := laag(last_day_moratorium, c(starts[i]:lag_counts[i])))
}

## binning endpoints cumulatively
last_lag <- paste0("beta_lag_", lags)
es <- es %>% 
  relocate(starts_with("beta_lag_"), treatment, date, university) %>% 
  ungroup() %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(beta_lag_binned = cumsum(!!sym(last_lag))) %>% 
  relocate(beta_lag_binned) %>% 
  ungroup()
  

## binning endpoints cumulatively
first_lead <- paste0("beta_lead_", leads)
es <- es %>% 
  relocate(starts_with("beta_lead_"), treatment, date, university) %>% 
  ungroup() %>% 
  group_by(university) %>% 
  arrange(desc(date)) %>% 
  mutate(beta_lead_binned = cumsum(!!sym(first_lead))) %>% 
  relocate(beta_lead_binned) %>% 
  ungroup()

## changing end points
es <- es %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(across(c(beta_lead_binned, beta_lag_binned), ~case_when(
    . >= 1 & . <= bin_length ~1,
    . > bin_length & . <=bin_length *2 ~ 2,
    . > bin_length*2 ~3,
    TRUE ~as.double(0))
  )) %>% 
  ungroup()


explanatory_vars <- c("beta_lead_binned", "beta_lead_7", "beta_lead_6", "beta_lead_5", "beta_lead_4", "beta_lead_3", "beta_lead_2", "treatment",
                      "beta_lag_1", "beta_lag_2", "beta_lag_3", "beta_lag_4", "beta_lag_5", "beta_lag_6", "beta_lag_7", "beta_lag_binned")

explanatory_vars_2 <- c("beta_lead_binned",  "beta_lead_3", "beta_lead_2", "treatment",
                        "beta_lag_1", "beta_lag_2", "beta_lag_3", "beta_lag_binned")
fe <- c("day_of_week", "university_by_academic_year_by_semester")

fixed_effects_0 <- c("day_of_week", "academic_year", "spring_semester", "university")
fixed_effects_1 <- c("day_of_week", "semester_by_academic_year", "university")
fixed_effects_2 <- c("day_of_week", "university_by_academic_year_by_semester")
fixed_effects_3 <- c("day_of_week_by_semester_by_academic_year", "university_by_academic_year_by_semester")
daily_fixed_effects = list(fixed_effects_0,fixed_effects_1, fixed_effects_2, fixed_effects_3)


x <- map(daily_fixed_effects, ~ifc::reghdfe(es, "alcohol_offense_per25", explanatory_vars_2, ., cluster = "university")) 
# %>% filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun")
ifc::event_study_graph(x[[4]], 4)
