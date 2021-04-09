library(tidyverse)
library(fixest)
library(modelsummary)
library(ifc)
library(lubridate)
library(broom)

weekly_crime <- read_csv("Created Data/xMaster_data_2021/weekly_panel.csv")

es <- ifc::event_study_week(weekly_crime, 8, reference_week = 1)

clean_es <- function(data) {
  cleaned_es <- data %>% 
    mutate(across(c(sexual_assault, alcohol_offense,
                    theft, robbery_burglary, drug_offense), list(ihs = ifc::ihs_transform),
                  .names = "{.fn}_{.col}")) 
  return(cleaned_es)
}

event_study_func <- function(x, window_size) {
  coefs <- tidy(x, conf.int =T)[1:(2*window_size), ]
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
    geom_path(linetype = "dashed", alpha = 0.8) +
    geom_point() +
    geom_errorbar(aes(x = value, ymin = conf.low, ymax = conf.high), alpha = 0.8) +
    geom_rect(aes(xmin = -0.5, xmax =0.5, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.03) +
    scale_x_continuous(labels = c(-window_size:window_size), breaks = c(-window_size:window_size)) +
    labs(x = "", y = "") +
    ggthemes::theme_calc()
  return(plot)
}

event_study_func(alcohol_es, 8) +
  expand_limits(y = c(-.95, .75)) 

es <- clean_es(es)

ihs_alcohol_es <- es %>% 
  feols(ihs_alcohol_offense ~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
                    treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
                    treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
                    treatment_plus_7 + treatment_plus_8 +  ftime_total_undergrad + total_undergrad_asian +
                    total_undergrad_hispanic + total_undergrad_black + graduation_rate_total_cohort_| uni_month +  year , cluster = ~university, data = .) 

per100_alcohol_es <- es %>% 
  feols(alcohol_offense_per100 ~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
          treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
          treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
          treatment_plus_7 + treatment_plus_8 +  ftime_total_undergrad + total_undergrad_asian +
          total_undergrad_hispanic + total_undergrad_black + graduation_rate_total_cohort_| uni_month +  year , cluster = ~university, data = .) 


ihs_sexual_assault_es <- es %>% 
  feols(ihs_sexual_assault ~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
          treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
          treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
          treatment_plus_7 + treatment_plus_8 +  ftime_total_undergrad + total_undergrad_asian +
          total_undergrad_hispanic + total_undergrad_black + graduation_rate_total_cohort_| uni_month +  year , cluster = ~university, data = .) 
per100_sexual_assault_es <- es %>% 
  feols(sexual_assault_per100 ~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
          treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
          treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
          treatment_plus_7 + treatment_plus_8 +  ftime_total_undergrad + total_undergrad_asian +
          total_undergrad_hispanic + total_undergrad_black + graduation_rate_total_cohort_| uni_month +  year , cluster = ~university, data = .) 

alcohol_es_100 <- event_study_func(per100_alcohol_es, 8) + 
  expand_limits(y = c(-20, 20))
alcohol_es_ihs <- event_study_func(ihs_alcohol_es, 8) +
  expand_limits(y = c(-1, 1))

sexual_assault_es_100 <- event_study_func(per100_sexual_assault_es, 8) + 
  expand_limits(y = c(-7, 5))
sexual_assault_es_ihs <- event_study_func(ihs_sexual_assault_es, 8)  + 
  expand_limits(y = c(-1,1))
