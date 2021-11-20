## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-11-10
##

library(tidyverse)
library(fixest)
library(modelsummary)

## create an academic year variable
## aggregate by university the percentage of days within an academic year
## split into quartiles
## run analysis on those quartiles

if (!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}



moratorium_ids <- daily_crime %>% 
  group_by(university) %>% 
  mutate(treatment_na = ifelse(treatment == 0, NA, treatment)) %>% 
  mutate(treatment_na = ifelse(treatment_na > 0 & (!is.na(closure_2)) & date >= closure_2, 2, treatment_na)) %>% 
  mutate(treatment_na = ifelse(is.na(treatment_na), 0, treatment_na)) %>% 
  ungroup() %>% 
  filter(treatment_na >0) %>% 
  group_by(treatment_na, university) %>% 
  mutate(moratorium_id = cur_group_id()) %>% 
  ungroup() %>% 
  select(moratorium_id, university, treatment,date)

daily_crime <- daily_crime %>% 
  left_join(moratorium_ids) %>% 
  mutate(moratorium_id = ifelse(is.na(moratorium_id), 0, moratorium_id))


moratorium_lengths <- daily_crime %>% 
  group_by(moratorium_id) %>% 
  mutate(length_moratorium = sum(treatment)) %>% 
  select(moratorium_id, length_moratorium, university) %>% 
  ungroup() %>% 
  filter(moratorium_id != 0) %>% 
  distinct(length_moratorium, university, moratorium_id) 

quartiles <- quantile(moratorium_lengths$length_moratorium, c(0.33, .66, 1))

daily_crime <- daily_crime %>% 
  left_join(moratorium_lengths) %>% 
  mutate(length_moratorium = ifelse(is.na(length_moratorium), 0, length_moratorium)) %>% 
  mutate(below_q33 = if_else(between(length_moratorium, 0.1,quartiles[[1]]), 1,0)) %>% 
  mutate(between_q33_q66 = ifelse(between(length_moratorium, quartiles[[1]], quartiles[[2]]), 1, 0)) %>% 
  mutate(above_q66 = ifelse(between(length_moratorium, quartiles[[2]], quartiles[[3]]), 1, 0)) 



# regressions -------------------------------------------------------------

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-by-Year", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: year", "FE: Year", ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
              "FE: date", "FE: Day-by-Month-by-Year", ~fmt,
              "FE: university_by_year_by_semester_number", "FE: University-by-Year-by-Semester-Number", ~fmt)

explanatory_vars <- c( "treatment:below_q33",
                           "treatment:between_q33_q66", "treatment:above_q66")
explanatory_vars_flex <- c("lead_2", "lead_1", "treatment:below_q33",
                      "treatment:between_q33_q66", "treatment:above_q66", 
                      "lag_1", "lag_2")

fe <- c("university", "date")


outcomes <- list("alcohol_offense_per25", "drug_offense_per25", "sexual_assault_per25")


# without leads and lags --------------------------------------------------

quantile_estimates<- map(outcomes, ~ifc::reghdfe(daily_crime , .,explanatory_vars, fe, "university"))
names(quantile_estimates) <- c("Alcohol Offense", "Drug Offense", "Sexual Assault")


quantile_moratorium <- quantile_estimates_ll %>% modelplot(coef_map = c("treatment:below_q33" = "Moratorium Length:
33rd Percentile",
"treatment:between_q33_q66" = "Moratorium Length: 
33rd-66th Percentile",
"treatment:above_q66" = "Moratorium Length:
Above 66th Percentile")) +
  geom_vline(xintercept  = 0, linetype = "dotted", color = "red") +
  coord_flip() +
  facet_grid(~model) +
  scale_color_manual(values=c("black", "black", "black")) +
  theme_minimal() +
  theme(legend.position = "none")


# with leads/lags ---------------------------------------------------------

quantile_estimates_ll<- map(outcomes, ~ifc::reghdfe(daily_crime , .,explanatory_vars_flex, fe, "university"))
names(quantile_estimates_ll) <- c("Alcohol Offense", "Drug Offense", "Sexual Assault")


quantile_moratorium_ll <- quantile_estimates_ll %>% modelplot(coef_map = c("lead_2" = "-2",
                                                                           "lead_1" = "-1","treatment:below_q33" = "33rd",
                         "treatment:between_q33_q66" = "33rd-66th",
                         "treatment:above_q66" = "66th", "lag_1" = "1", "lag_2" = "2")) +
  geom_vline(xintercept  = 0, linetype = "dotted", color = "red") +
  coord_flip() +
  facet_grid(~model) +
  scale_color_manual(values=c("black", "blue", "red")) +
  theme_minimal() +
  theme(legend.position = "none")
  

