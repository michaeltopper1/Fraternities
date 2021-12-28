
## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-07-23
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(lubridate)
library(kableExtra)
library(ggrepel)

if(!exists("daily_crime_deaths")) {
  daily_crime_deaths <- read_csv("created_data/xmaster_data/daily_panel_allschools.csv") %>% 
    filter(university %in% ifc::moratorium_schools() | university %in% ifc::death_untreated_universities())
}

daily_crime_deaths_weekends <- daily_crime_deaths %>% 
  filter(day_of_week == "Sat" | day_of_week == "Sun" | day_of_week == "Fri")

daily_crime_deaths_weekdays <- daily_crime_deaths %>% 
  filter(day_of_week == "Mon" | day_of_week == "Tue" | day_of_week == "Wed" | day_of_week == "Thu")

daily_crime_deaths <- daily_crime_deaths %>% 
  mutate(across(c(reason1, reason2), ~ifelse(is.na(.), "untreated", .))) %>% 
  mutate(reason_sexual_assault = ifelse((reason1 == 'sexual assault' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
                                        | (reason2 == 'sexual assault' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>% 
  mutate(reason_death = ifelse((reason1 == 'death' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
                               | (reason2 == 'death' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>% 
  mutate(reason_behavior = ifelse((reason1 == 'behavior' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
                                  | (reason2 == 'behavior' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>% 
  mutate(reason_unknown = ifelse((reason1 == 'unknown' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
                                 | (reason2 == 'unknown' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0))

daily_crime_deaths_weekends <- daily_crime_deaths_weekends %>%
  mutate(across(c(reason1, reason2), ~ifelse(is.na(.), "untreated", .))) %>%
  mutate(reason_sexual_assault = ifelse((reason1 == 'sexual assault' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                                        | (reason2 == 'sexual assault' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>%
  mutate(reason_death = ifelse((reason1 == 'death' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                               | (reason2 == 'death' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>%
  mutate(reason_behavior = ifelse((reason1 == 'behavior' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                                  | (reason2 == 'behavior' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>%
  mutate(reason_unknown = ifelse((reason1 == 'unknown' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                                 | (reason2 == 'unknown' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0))
daily_crime_deaths_weekdays <- daily_crime_deaths_weekdays %>% 
  mutate(across(c(reason1, reason2), ~ifelse(is.na(.), "untreated", .))) %>%
  mutate(reason_sexual_assault = ifelse((reason1 == 'sexual assault' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                                        | (reason2 == 'sexual assault' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>%
  mutate(reason_death = ifelse((reason1 == 'death' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                               | (reason2 == 'death' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>%
  mutate(reason_behavior = ifelse((reason1 == 'behavior' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                                  | (reason2 == 'behavior' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>%
  mutate(reason_unknown = ifelse((reason1 == 'unknown' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                                 | (reason2 == 'unknown' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0))

offenses <- list("alcohol_offense_per25")

explanatory_vars <- c("treatment:reason_sexual_assault",
                      "treatment:reason_death",
                      "treatment:reason_behavior",
                      "treatment:reason_unknown")

fe <- c( "day_of_week", "university_by_academic_year", "holiday", "spring_semester")
death_regs <- map(offenses, ~ifc::reghdfe(daily_crime_deaths,  ., explanatory_vars ,fe , "university"))

death_regs_weekends <- map(offenses, ~ifc::reghdfe(daily_crime_deaths_weekends,  ., explanatory_vars
                                                      , fe, "university"))

death_regs_weekdays <- map(offenses, ~ifc::reghdfe(daily_crime_deaths_weekdays,  ., explanatory_vars
                                                      , fe, "university"))


death_regs_weekends
