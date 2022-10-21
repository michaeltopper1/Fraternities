## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-10-21
##

library(tidyverse)
library(lubridate)
library(fixest)
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

explanatory_vars <- c("treatment")

fixed_effects_2 <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")

alc <- ifc::reghdfe(daily_crime_nevertreated_pseudo, c("alcohol_offense_per25"),explanatory_vars, fixed_effects_2, "university")
