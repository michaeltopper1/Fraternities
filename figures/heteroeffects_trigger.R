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

if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv") 
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}


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

# daily_crime_het_weekends <- daily_crime_weekends %>% 
#   mutate(across(c(reason1, reason2), ~ifelse(is.na(.), "untreated", .))) %>% 
#   mutate(reason_sexual_assault = ifelse((reason1 == 'sexual assault' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
#                                         | (reason2 == 'sexual assault' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>% 
#   mutate(reason_death = ifelse((reason1 == 'death' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
#                                | (reason2 == 'death' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>% 
#   mutate(reason_behavior = ifelse((reason1 == 'behavior' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
#                                   | (reason2 == 'behavior' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>% 
#   mutate(reason_unknown = ifelse((reason1 == 'unknown' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
#                                  | (reason2 == 'unknown' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0))

offenses <- list("alcohol_offense_per25", "drug_offense_per25", "sexual_assault_per25")

offenses_regs <- map(offenses, ~ifc::reghdfe(daily_crime_het,  ., c("lead_2", "lead_1","treatment:reason_sexual_assault",
                                                                      "treatment:reason_death",
                                                                      "treatment:reason_behavior",
                                                                      "treatment:reason_unknown",
                                                                      "lag_1", "lag_2"), c("university", "date"), "university"))

# offenses_regs_weekends <- map(offenses, ~ifc::reghdfe(daily_crime_het_weekends,  ., c("lead_2", "lead_1","treatment:reason_sexual_assault",
#                                                                     "treatment:reason_death",
#                                                                     "treatment:reason_behavior",
#                                                                     "treatment:reason_unknown",
#                                                                     "lag_1", "lag_2"), c("university", "date"), "university"))
names(offenses_regs) <- c("Alcohol", "Drug", "Sexual Assault")

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-by-Year", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: year", "FE: Year", ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
              "FE: date", "FE: Day-by-Month-by-Year", ~fmt,
              "FE: university_by_year_by_semester_number", "FE: University-by-Year-by-Semester-Number", ~fmt)

trigger_table <- modelsummary(offenses_regs, stars = T,
             gof_omit = 'DF|Deviance|AIC|BIC|Log|R2 Within|R2 Ps|R2|R2 Adj.',
             coef_rename = c("treatment:reason_sexual_assault" = "Moratorium x Triggering Sexual Assault",
                             "treatment:reason_death" = "Moratorium x Triggering Death of Student",
                             "treatment:reason_behavior" = "Motatorium x Triggering Behavior Violation",
                             "treatment:reason_unknown" = "Moratorium x Triggering Event Unknown",
                             "lead_2" = "2 Weeks Before",
                             "lead_1" = "1 Week Before",
                             "lag_1" = "1 Week After",
                             "lag_2" = "2 Weeks After"),
             gof_map = gm,
             notes = list("Standard errors clustered by  university.",
                          "Offenses are per-25000 enrolled students.",
                          "Columns represent the dependent variable.",
                          "Weekends look similar except sexual assault - effects driven by weekdays."),
             title = "\\label{trigger_table}Effect of Moratoriums on Offenses by Triggering Event") %>% 
  add_header_above(c(" " = 1, "Dependent Variable" = 3))
