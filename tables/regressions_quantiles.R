## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-11-10
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

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
  mutate(treatment_na = ifelse(treatment_na > 1 & (!is.na(closure_3)) & date >= closure_3, 3, treatment_na)) %>% 
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

outcomes <- c("alcohol_offense_per25",
                      "sexual_assault_per25")

fe <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester")


explanatory_vars <- list("treatment:below_q33",
                 "treatment:between_q33_q66", "treatment:above_q66")


# without leads and lags --------------------------------------------------

quantile_estimates_panel_a <- map(outcomes, ~ifc::reghdfe(daily_crime , .,"treatment:below_q33", fe, "university"))
quantile_estimates_panel_b <- map(outcomes, ~ifc::reghdfe(daily_crime , .,"treatment:between_q33_q66", fe, "university"))
quantile_estimates_panel_c <- map(outcomes, ~ifc::reghdfe(daily_crime , .,"treatment:above_q66", fe, "university"))




quantile_table <- ifc::main_table(quantile_estimates_panel_a,quantile_estimates_panel_b, last_panel = quantile_estimates_panel_c) %>% 
  slice(1:9) %>% 
  kbl(booktabs = T, col.names = c(" ", "Alcohol Offenses", "Sexual Assaults"),
      caption = "\\label{quantile_table}Effect of Moratoriums by Moratorium Length") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  pack_rows("Panel A: Below 33rd Percentile in Length", 1, 3) %>% 
  pack_rows("Panel B: Between 33rd and 66th Percentile in Length", 4, 6) %>% 
  pack_rows("Panel C: Above 66th Percentile in Length", 7, 9) %>% 
  add_header_above(c(" " = 1, "Type of Offense" = 2)) %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as per-25000 enrolled students. Each panel represents a subset of moratoriums that were split by three quantiles based on moratorium length: below the 33rd percentile, between the 33rd and 66th percentile, and above the 66th percentile. Controls include day of week, spring semester, holiday, and university by academic year. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since no university's academic calendar contains them. A moratorium is a temporary halt on fraternity-related activities with alcohol.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"),
           threeparttable = T) 


