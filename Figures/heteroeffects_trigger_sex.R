## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-06-23
##
## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-06-23
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


hetero_reasons_sex <- daily_crime_het %>% 
  feols(c(sexual_assault_per25) ~ sw(treatment:reason_sexual_assault,
                                      treatment:reason_death,
                                      treatment:reason_behavior,
                                      treatment:reason_unknown) |
          uni_semester + weekday, cluster = ~university, data = .) 
names(hetero_reasons_sex) <- c("Sexual Assault", "Death of Student", "Behavior Violation", "Unknown")

mean_sex <- mean(daily_crime$sexual_assault_per25, na.rm = T)
mean_of_sex <- tribble(~sex, ~new, ~x, ~y, ~z,
                       "Mean of Sexual Assault Per 25k ",mean_sex, mean_sex, mean_sex, mean_sex)
attr(mean_of_sex, 'position') <- c(10)


heteroeffects_sex <- hetero_reasons_sex %>% 
  modelsummary(stars = T, gof_omit ="^R|^AIC|^BIC|^Log",
               coef_rename = c("treatment:reason_sexual_assault" = "Moratorium x Triggering Sexual Assault",
                               "treatment:reason_death" = "Moratorium x Triggering Death of Student",
                               "treatment:reason_behavior" = "Motatorium x Triggering Behavior Violation",
                               "treatment:reason_unknown" = "Moratorium x Triggering Event Unknown"),
               title = "Effect of fraternity moratoria on sexual assault by triggering event.", add_rows = mean_of_sex) %>% 
  kableExtra::add_header_above(c(" " = 1, "(1)" =1, "(2)" = 1, "(3)" = 1, "(4)" = 1)) %>% 
  kableExtra::add_header_above(c(" " = 1, "Triggering Event" = 4))
