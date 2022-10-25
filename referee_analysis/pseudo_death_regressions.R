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

daily_crime_nevertreated_pseudo %>% 
  feols(alcohol_offense_per25 ~ treatment |
          day_of_week + university_by_academic_year + holiday + spring_semester + game_occurred,
        cluster = ~university,
        weights = NULL,
        data = .)

alc_pseudo <- ifc::reghdfe(daily_crime_nevertreated_pseudo, c("alcohol_offense_per25"),
                           explanatory_vars, fixed_effects_2, "university",
                           weights = daily_crime_nevertreated_pseudo$total_enrollment)

alc_pseudo_weekends <- ifc::reghdfe(daily_crime_nevertreated_pseudo_weekends, c("alcohol_offense_per25"),
             explanatory_vars, fixed_effects_2, "university",
             weights = daily_crime_nevertreated_pseudo_weekends$total_enrollment)

alc_pseudo_weekdays <- ifc::reghdfe(daily_crime_nevertreated_pseudo_weekdays, c("alcohol_offense_per25"),
             explanatory_vars, fixed_effects_2, "university",
             weights = daily_crime_nevertreated_pseudo_weekdays$total_enrollment)


sex_pseudo <- ifc::reghdfe(daily_crime_nevertreated_pseudo, 
                           c("sexual_assault_per25"),
                           explanatory_vars, 
                           fixed_effects_2, 
                           "university")
sex_pseudo_weekends <- ifc::reghdfe(daily_crime_nevertreated_pseudo_weekends, 
             c("sexual_assault_per25"),
             explanatory_vars, 
             fixed_effects_2, 
             "university",
             weights = daily_crime_nevertreated_pseudo_weekends$total_enrollment)

sex_pseudo_weekdays <- ifc::reghdfe(daily_crime_nevertreated_pseudo_weekdays, 
             c("sexual_assault_per25"),
             explanatory_vars, 
             fixed_effects_2, 
             "university",
             weights = daily_crime_nevertreated_pseudo_weekdays$total_enrollment)
gof_mapping <- ifc::gof_mapping() %>% 
  select(-fmt) %>% 
  mutate(fmt = ifelse(raw == "nobs", 0, 3)) %>% 
  add_row(raw = "mean", clean = "Mean of Dependent Variable", fmt = 3, .after = 1)
panelsummary(list(alc_pseudo, alc_pseudo_weekdays, alc_pseudo_weekends),
             list(sex_pseudo, sex_pseudo_weekdays, sex_pseudo_weekends),
             collapse_fe = T, stars = T, 
             panel_labels = c("Panel A: Alcohol Offenses", "Panel B: Sexual Assaults"),
             coef_map = c("treatment" = "64-Day Death Period"),
             gof_map = gof_mapping,
             mean_dependent = T)
