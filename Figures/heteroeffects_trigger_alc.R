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

if(!exists("daily_crime")) {
  daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv")
}

weekends <- c("Fri", "Sat", "Sun")

daily_crime_het <- daily_crime %>% 
  mutate(across(c(reason1, reason2), ~ifelse(is.na(.), "untreated", .))) %>% 
  mutate(reason_sexual_assault = ifelse((reason1 == 'sexual assault' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
                                        | (reason2 == 'sexual assault' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>% 
  mutate(reason_death = ifelse((reason1 == 'death' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
                               | (reason2 == 'death' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>% 
  mutate(reason_behavior = ifelse((reason1 == 'behavior' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
                                  | (reason2 == 'behavior' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>% 
  mutate(reason_unknown = ifelse((reason1 == 'unknown' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
                                 | (reason2 == 'unknown' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0))

hetero_reasons_death <- daily_crime_het %>% 
  feols(c(alcohol_offense_per25) ~ sw(treatment:reason_sexual_assault,
                                     treatment:reason_death,
                                     treatment:reason_behavior,
                                     treatment:reason_unknown) |
          uni_semester + weekday, cluster = ~university, data = .) 
names(hetero_reasons_death) <- c("Sexual Assault", "Death of Student", "Behavior Violation", "Unknown")

mean_alcohol <- mean(daily_crime$alcohol_offense_per25, na.rm = T)
mean_alcohol <- tribble(~sex, ~new, ~x, ~y, ~z,
                       "Mean of Alcohol Offense Per 25k ",mean_alcohol, mean_alcohol, mean_alcohol, mean_alcohol)
attr(mean_alcohol, 'position') <- c(10)

heteroeffects_alc <- hetero_reasons_death %>% 
  modelsummary(stars = T, gof_omit ="^R|^AIC|^BIC|^Log",
               coef_rename = c("treatment:reason_sexual_assault" = "Moratorium x Triggering Sexual Assault",
                               "treatment:reason_death" = "Moratorium x Triggering Death of Student",
                               "treatment:reason_behavior" = "Motatorium x Triggering Behavior Violation",
                               "treatment:reason_unknown" = "Moratorium x Triggering Event Unknown"),
               title = "Effect of fraternity moratoriums on alcohol offenses by triggering event.", add_rows = mean_alcohol) %>%
  kableExtra::add_header_above(c(" " = 1, "(1)" =1, "(2)" = 1, "(3)" = 1, "(4)" = 1)) %>% 
  kableExtra::add_header_above(c(" " = 1, "Triggering Event" = 4))
