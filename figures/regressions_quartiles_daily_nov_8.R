## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-11-10
##

library(tidyverse)
library(fixest)

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

quartiles <- quantile(moratorium_lengths$length_moratorium, c(0.25, 0.5, 0.75, 1))

daily_crime <- daily_crime %>% 
  left_join(moratorium_lengths) %>% 
  mutate(length_moratorium = ifelse(is.na(length_moratorium), 0, length_moratorium)) %>% 
  mutate(below_q25 = if_else(between(length_moratorium, 0.1,quartiles[[1]]), 1,0)) %>% 
  mutate(between_q25_q50 = ifelse(between(length_moratorium, quartiles[[1]], quartiles[[2]]), 1, 0)) %>% 
  mutate(between_q50_q75 = ifelse(between(length_moratorium, quartiles[[2]], quartiles[[3]]), 1, 0)) %>% 
  mutate(above_q75 = ifelse(between(length_moratorium, quartiles[[3]], quartiles[[4]]), 1, 0))



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

length_full <- daily_crime %>% 
  # filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun") %>% 
  feols(c(alcohol_offense_per25, drug_offense_per25, sexual_assault_per25) ~ lead_2 + lead_1 + treatment:below_q25 + treatment:between_q25_q50 +
          treatment:between_q50_q75 + treatment:above_q75 + lag_1 + lag_2 |
          university + date, cluster = ~university, data = .) %>% 
  modelsummary(stars = T,
               gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
               coef_omit = "^l", output = "data.frame",
               gof_map = gm) %>% 
  mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>% 
  select(-part, - statistic)

length_weekend <- daily_crime %>% 
  filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun") %>%
  feols(c(alcohol_offense_per25, drug_offense_per25, sexual_assault_per25) ~ lead_2 + lead_1 + treatment:below_q25 + treatment:between_q25_q50 +
          treatment:between_q50_q75 + treatment:above_q75 + lag_1 + lag_2 |
          university + date, cluster = ~university, data = .) %>% 
  modelsummary(stars = T,
               gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
               coef_omit = "^l", output = "data.frame",
               gof_map = gm) %>% 
  mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>% 
  select(-part, - statistic, - term)


# create table ------------------------------------------------------------


length_table <- bind_cols(length_full, length_weekend)

length_table <- length_table %>% 
  kbl(col.names = c(" ", "Alcohol", "Drug", "Sexual Assault", "Alcohol", "Drug", "Sexual Assault"),
      booktabs = T,
      caption = "\\label{hetero_length} Heterogeneous Effects: Difference in Lengths") %>% 
  add_header_above(c(" " = 1, "Full Sample" = 3, "Weekends" = 3)) %>% 
  kable_paper() %>% 
  footnote(list("Standard errors clustered by university.", 
                "2 leads and 2 lags for week before are included in the specification.",
                "Each estimate represents a different percentile.",
                "Percentiles are 25th, 50th, 75th"))




# lag differentials by length ---------------------------------------------

weekend_lags <- daily_crime %>% 
  filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun") %>% 
  feols(c(alcohol_offense_per25, drug_offense_per25, sexual_assault_per25) ~treatment +lag_1:below_q25 + lag_1:between_q25_q50 +
          lag_1:between_q50_q75 + lag_1:above_q75 |
          university + date , cluster = ~university, data = .) %>% 
  modelplot(coef_rename = c("treatment" = "Moratorium",
                            "alcohol_offense_per25" = "Alcohol Offense")) +
  geom_vline(xintercept = 0, color = "red", alpha = 0.4) +
  labs(caption = "Fixed effects are university and day-by-month-by-year.")

full_sample_lags <- daily_crime %>% 
  feols(c(alcohol_offense_per25, drug_offense_per25, sexual_assault_per25) ~treatment +lag_1:below_q25 + lag_1:between_q25_q50 +
          lag_1:between_q50_q75 + lag_1:above_q75 |
          university + date , cluster = ~university, data = .) %>% 
  modelplot(coef_rename = c("treatment" = "Moratorium",
                            "alcohol_offense_per25" = "Alcohol Offense")) +
  geom_vline(xintercept = 0, color = "red", alpha = 0.4) +
  labs(caption = "Fixed effects are university and day-by-month-by-year.")

