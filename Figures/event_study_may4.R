

library(tidyverse)
library(fixest) 
library(modelsummary)
library(ifc)
library(lubridate)
library(kableExtra)

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv",
                        guess_max = 50000)

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
    geom_path(linetype = "dashed", alpha = 0.8) +
    geom_point() +
    geom_errorbar(aes(x = value, ymin = conf.low, ymax = conf.high), alpha = 0.8) +
    geom_rect(aes(xmin = -0.5, xmax =0.5, ymin = -Inf, ymax = Inf), fill = "grey", alpha = 0.03) +
    scale_x_continuous(labels = c(-window_size:window_size), breaks = c(-window_size:window_size)) +
    labs(x = "", y = "") +
    ggthemes::theme_calc()
  return(plot)
}


es <- ifc::event_study_stagger(daily_crime, 8, 7, 1)

es <- es %>% 
  mutate(across(starts_with("treatment"), ~ ifelse(university %in% ifc::untreated_universities(), 0, . ))) 


es_alc_pois <- es %>% 
  fepois(alcohol_offense~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
           treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
           treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
           treatment_plus_7 + treatment_plus_8 +  ftime_total_undergrad + total_undergrad_asian +
           total_undergrad_hispanic + total_undergrad_black + graduation_rate_total_cohort_| 
           university + month + year + weekday, cluster = ~university, data = .)
es_alc_ols <- es %>% 
  fepois(alcohol_offense_per25~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
           treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
           treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
           treatment_plus_7 + treatment_plus_8 +  ftime_total_undergrad + total_undergrad_asian +
           total_undergrad_hispanic + total_undergrad_black + graduation_rate_total_cohort_| 
           university + month + year + weekday, cluster = ~university, data = .)

es_sex_pois <- es %>% 
  fepois(sexual_assault~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
           treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
           treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
           treatment_plus_7 + treatment_plus_8 +  ftime_total_undergrad + total_undergrad_asian +
           total_undergrad_hispanic + total_undergrad_black + graduation_rate_total_cohort_| 
           university + month + year + weekday, cluster = ~university, data = .)
es_sex_ols <- es %>% 
  fepois(sexual_assault_per25~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
           treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
           treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
           treatment_plus_7 + treatment_plus_8 +  ftime_total_undergrad + total_undergrad_asian +
           total_undergrad_hispanic + total_undergrad_black + graduation_rate_total_cohort_| 
           university + month + year + weekday, cluster = ~university, data = .)

alcohol_es_pois <- event_study_func(es_alc_pois, 8) 
alcohol_es_ols <- event_study_func(es_alc_ols, 8) 
sex_es_pois <- event_study_func(es_sex_pois, 8)
sex_es_ols <- event_study_func(es_sex_ols, 8)


es_alc_pois_uni_month <- es %>% 
  fepois(alcohol_offense~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
           treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
           treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
           treatment_plus_7 + treatment_plus_8 +  ftime_total_undergrad + total_undergrad_asian +
           total_undergrad_hispanic + total_undergrad_black + graduation_rate_total_cohort_| 
           uni_month + year + weekday, cluster = ~university, data = .)
es_alc_ols_uni_month <- es %>% 
  fepois(alcohol_offense_per25~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
           treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
           treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
           treatment_plus_7 + treatment_plus_8 +  ftime_total_undergrad + total_undergrad_asian +
           total_undergrad_hispanic + total_undergrad_black + graduation_rate_total_cohort_| 
           uni_month + year + weekday, cluster = ~university, data = .)

es_sex_pois_uni_month <- es %>% 
  fepois(sexual_assault~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
           treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
           treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
           treatment_plus_7 + treatment_plus_8 +  ftime_total_undergrad + total_undergrad_asian +
           total_undergrad_hispanic + total_undergrad_black + graduation_rate_total_cohort_| 
           uni_month + year + weekday, cluster = ~university, data = .)
es_sex_ols_uni_month <- es %>% 
  fepois(sexual_assault_per25~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
           treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
           treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
           treatment_plus_7 + treatment_plus_8 +  ftime_total_undergrad + total_undergrad_asian +
           total_undergrad_hispanic + total_undergrad_black + graduation_rate_total_cohort_| 
           uni_month + year + weekday, cluster = ~university, data = .)

alcohol_es_pois_uni_month <- event_study_func(es_alc_pois_uni_month, 8) 
alcohol_es_ols_uni_month <- event_study_func(es_alc_ols_uni_month, 8) 
sex_es_pois_uni_month <- event_study_func(es_sex_pois_uni_month, 8)
sex_es_ols_uni_month <- event_study_func(es_sex_ols_uni_month, 8)
