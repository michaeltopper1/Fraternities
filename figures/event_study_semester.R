## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-11-04
##

library(tidyverse)
library(lubridate)
library(fixest)

if (!exists("semester_level")) {
  semester_level <- read_csv("created_data/xmaster_data/semester_level.csv")
}

if (!exists("semester_level_weekdays")){
  semester_level_weekdays <- read_csv("created_data/xmaster_data/semester_level_weekdays.csv")
}

if (!exists("semester_level_weekends")) {
  semester_level_weekends <- read_csv("created_data/xmaster_data/semester_level_weekends.csv")
}


lead_endpoint <- 6
lag_endpoint <- 6
leads <- c(1:lead_endpoint)
lags <- c(1:lag_endpoint)

closure_table <- readxl::read_excel("data/closure_spreadsheet_final_2019.xlsx") %>% 
  janitor::clean_names() %>% 
  rename(closure_1 = date, closure_2 = date2, closure_1_end = deadline, closure_2_end = deadline2)

es_semester <- semester_level



es_semester %>% 
  relocate(starts_with("beta"), treatment) %>% View()


# creating leads and lags -------------------------------------------------

for (i in leads) {
  column_name <- paste0("beta_lead_",i)
  es_semester <- es_semester %>% 
    group_by(university) %>% 
    arrange(semester_number) %>% 
    mutate(!!sym(column_name) := dplyr::lead(treatment, n = i))
}

for (i in lags) {
  column_name <- paste0("beta_lag_",i)
  es_semester <- es_semester %>% 
    group_by(university) %>% 
    arrange(semester_number) %>% 
    mutate(!!sym(column_name) := dplyr::lag(treatment, n = i))
}

es_semester <- es_semester %>% 
  mutate(across(starts_with("beta"), ~ifelse(is.na(.), 0, .)))



## getting the first lead to be binned
last_lead <- paste0("beta_lead_", lead_endpoint)
es_semester <- es_semester %>% 
  group_by(university) %>% 
  arrange(desc(semester_number)) %>% 
  mutate(beta_lead_binned = ifelse(!!sym(last_lead) ==1, 1, NA)) %>% 
  mutate(beta_lead_binned = ifelse(!!sym(last_lead) > 0, cumsum(!!sym(last_lead)), NA)) %>% 
  relocate(beta_lead_binned, starts_with("beta")) %>% 
  fill(beta_lead_binned, .direction = "down")  %>% ungroup() %>% arrange(semester_number)


## getting the last lag to be binned
last_lag <- paste0("beta_lag_", lag_endpoint)
es_semester <- es_semester %>% 
  group_by(university) %>% 
  arrange(university,semester_number) %>% 
  mutate(beta_lag_binned = ifelse(!!sym(last_lag) == 1, 1, NA)) %>% 
  mutate(beta_lag_binned = ifelse(!!sym(last_lag) > 0, cumsum(!!sym(last_lag)), NA)) %>% 
  relocate(beta_lag_binned) %>% 
  fill(beta_lag_binned, .direction = "down")  %>% ungroup()

es_semester <- es_semester %>% 
  mutate(across(c(beta_lead_binned, beta_lag_binned), ~ifelse(is.na(.), 0, .)))


# missing data ------------------------------------------------------------

es_semester <- es_semester %>% 
  mutate(across(c(sexual_assault_per25, sexual_assault,
                drug_offense, drug_offense_per25,
                alcohol_offense, alcohol_offense_per25), 
         ~if_else(year == 2014 & university == "Rollins College", NA_real_, .))) %>% 
  mutate(across(c(sexual_assault_per25, sexual_assault,
                drug_offense, drug_offense_per25,
                alcohol_offense, alcohol_offense_per25), 
         ~if_else(semester_number ==1 & university == "North Carolina State University at Raleigh", NA_real_, .))) 

# eventstudy graph function -----------------------------------------------

event_study_func <- function(x, window_size) {
  coefs <- broom::tidy(x, conf.int =T)[1:(2*window_size), ]
  coefs <- coefs %>% 
    add_row(.before = window_size)
  event_window <- c(-window_size:window_size)
  event_window <- as_tibble(event_window)  
  coefs <- tibble(coefs, event_window)
  coefs <- coefs %>% 
    mutate(across(.cols = -c(term), ~ ifelse(is.na(.), 0, .))) 
  coefs <- coefs[2:(2*window_size),]
  plot <- coefs %>% 
    ggplot(aes(x = value, y = estimate), alpha = 0.8) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),fill = "light grey", alpha =0.8) +
    geom_path(linetype = "dashed", color = "black") +
    geom_point() +
    # geom_errorbar(aes(x = value, ymin = conf.low, ymax = conf.high), alpha = 0.8) +
    # geom_rect(aes(xmin = -0.5, xmax =0.5, ymin = -Inf, ymax = Inf), fill = "light blue", alpha = 0.03) +
    scale_x_continuous(labels = c(-window_size:window_size), breaks = c(-window_size:window_size)) +
    labs(x = "Semesters to Moratorium", y = "Coefficient Estimate") +
    theme_minimal()
  return(plot)
}

alc_es <- es_semester %>% 
  feols(alcohol_offense_per25 ~beta_lead_binned+ beta_lead_5 + beta_lead_4 + beta_lead_3 + beta_lead_2 + treatment +
          beta_lag_1 + beta_lag_2 + beta_lag_3 + beta_lag_4 + beta_lag_5 + beta_lag_binned|university_by_semester_number + semester_number,
        cluster = ~university, data = .)
drug_es <- es_semester %>% 
  feols(drug_offense_per25 ~beta_lead_binned+ beta_lead_5 + beta_lead_4 + beta_lead_3 + beta_lead_2 + treatment +
          beta_lag_1 + beta_lag_2 + beta_lag_3 + beta_lag_4 + beta_lag_5 + beta_lag_binned |university_by_semester_number + semester_number,
        cluster = ~university, data = .)
sex_es <- es_semester %>% 
  feols(sexual_assault_per25 ~beta_lead_binned+ beta_lead_5 + beta_lead_4 + beta_lead_3 + beta_lead_2 + treatment +
          beta_lag_1 + beta_lag_2 + beta_lag_3 + beta_lag_4 + beta_lag_5 + beta_lag_binned |university_by_semester_number + semester_number,
        cluster = ~university, data = .)

event_study_func(alc_es, 6) +
  geom_hline(yintercept = 0, color = "dark red") +
  labs(title = "Alcohol Offenses")

event_study_func(drug_es, 6) +
  geom_hline(yintercept = 0, color = "dark red") +
  labs(title = 'Drug Offenses')

event_study_func(sex_es, 6) +
  geom_hline(yintercept = 0, color = "dark red")  +
  labs(title = "Sexual Assault Offenses")




es_semester %>%
  relocate(beta_lead_binned, beta_lead_3, beta_lead_2, beta_lead_1, treatment, beta_lag_1, beta_lag_2, beta_lag_3,
           beta_lag_binned,university,semester_number) 

