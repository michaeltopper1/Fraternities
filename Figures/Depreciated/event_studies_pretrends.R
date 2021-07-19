library(tidyverse)
library(lfe)
library(broom)
library(modelsummary)
library(kableExtra)

## input in your event study with window of 8
event_study_func <- function(x, window_size) {
  coefs <- tidy(x, conf.int =T)
  coefs <- coefs %>% 
    add_row(.before = window_size)
  event_window <- c(-window_size:window_size)
  event_window <- as_tibble(event_window)  
  coefs <- tibble(coefs, event_window)
  coefs <- coefs %>% 
    mutate(estimate =ifelse(is.na(estimate), 0, estimate)) 
  plot <- coefs %>% 
    ggplot(aes(x = value, y = estimate), alpha = 0.8) +
    geom_path(linetype = "dashed", alpha = 0.8) +
    geom_point() +
    geom_errorbar(aes(x = value, ymin = conf.low, ymax = conf.high), alpha = 0.8) + 
    geom_rect(aes(xmin = -0.5, xmax =0.5, ymin = -Inf, ymax = Inf), alpha = 0.03) +
    scale_x_continuous(labels = c(-window_size:window_size), breaks = c(-window_size:window_size)) +
    labs(x = "", y = "") +
    ggthemes::theme_clean()
  return(plot)
}



path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/xMaster_data_2021/event_study.csv"
event_study <- read_csv(path)
event_study <- event_study %>% 
  filter(university != "Miami University-Oxford" & university != "Emory University") 

universities <- event_study %>% 
  distinct(university) %>% pull()
## quarter system schools list
quarter_system_schools <- c("California Polytechnic State University-San Luis Obispo",
                            "Northwestern University")
## the complement of quarter system schools
semester_system_schools <- dplyr::setdiff(universities, quarter_system_schools)

event_study <- event_study %>% 
  filter(!(university %in% quarter_system_schools & (month == 7 | month == 8))) %>% 
  filter(!(university %in% semester_system_schools & (month == 6 | month == 7))) 



event_study <- event_study %>% 
  group_by(university, month) %>% 
  mutate(university_by_month = cur_group_id()) %>% 
  ungroup()
event_study <- event_study %>% 
  group_by(university,year) %>% 
  mutate(university_by_year = cur_group_id()) %>% 
  ungroup()

es_sexual_assault <- felm(sexual_assault  ~  treatment_minus_6 + 
                              treatment_minus_5 + treatment_minus_4 + 
                              treatment_minus_3 + treatment_minus_2 + 
                              treatment_1 + treatment_plus_1 +
                              treatment_plus_2 +treatment_plus_3 + 
                              treatment_plus_4 + treatment_plus_5 + 
                              treatment_plus_6  | university + date | 0 | university, data = event_study,
                          cmethod = 'reghdfe')
es_alcohol <- felm(alcohol_offense  ~  treatment_minus_6 + 
                            treatment_minus_5 + treatment_minus_4 + 
                            treatment_minus_3 + treatment_minus_2 + 
                            treatment_1 + treatment_plus_1 +
                            treatment_plus_2 +treatment_plus_3 + 
                            treatment_plus_4 + treatment_plus_5 + 
                            treatment_plus_6  | university + date | 0 | university, data = event_study,
                          cmethod = 'reghdfe')

es_drug <- felm(drug_offense  ~  treatment_minus_6 + 
                     treatment_minus_5 + treatment_minus_4 + 
                     treatment_minus_3 + treatment_minus_2 + 
                     treatment_1 + treatment_plus_1 +
                     treatment_plus_2 +treatment_plus_3 + 
                     treatment_plus_4 + treatment_plus_5 + 
                     treatment_plus_6  | university + date | 0 | university, data = event_study,
                   cmethod = 'reghdfe')
es_theft <- felm(theft  ~  treatment_minus_6 + 
                     treatment_minus_5 + treatment_minus_4 + 
                     treatment_minus_3 + treatment_minus_2 + 
                     treatment_1 + treatment_plus_1 +
                     treatment_plus_2 +treatment_plus_3 + 
                     treatment_plus_4 + treatment_plus_5 + 
                     treatment_plus_6  | university + date | 0 | university, data = event_study,
                   cmethod = 'reghdfe')
es_robbery <- felm(robbery_burglary  ~  treatment_minus_6 + 
                     treatment_minus_5 + treatment_minus_4 + 
                     treatment_minus_3 + treatment_minus_2 + 
                     treatment_1 + treatment_plus_1 +
                     treatment_plus_2 +treatment_plus_3 + 
                     treatment_plus_4 + treatment_plus_5 + 
                     treatment_plus_6  | university + date | 0 | university, data = event_study,
                   cmethod = 'reghdfe')
es_noise <- felm(noise_offense  ~  treatment_minus_6 + 
                     treatment_minus_5 + treatment_minus_4 + 
                     treatment_minus_3 + treatment_minus_2 + 
                     treatment_1 + treatment_plus_1 +
                     treatment_plus_2 +treatment_plus_3 + 
                     treatment_plus_4 + treatment_plus_5 + 
                     treatment_plus_6  | university + date | 0 | university, data = event_study,
                   cmethod = 'reghdfe')

event_study_sex <- event_study_func(es_sexual_assault, 6) +
  scale_y_continuous(limits = c(-4, 6))
event_study_alc <- event_study_func(es_alcohol, 6) +
  scale_y_continuous(limits = c(-30, 30))
event_study_drug <- event_study_func(es_drug, 6) +
  scale_y_continuous(limits = c(-20, 20))
event_study_theft <- event_study_func(es_theft, 6) +
  scale_y_continuous(limits = c(-30, 30))
event_study_robbery <- event_study_func(es_robbery, 6) +
  scale_y_continuous(limits = c(-10, 10))
event_study_noise <- event_study_func(es_noise, 6) +
  scale_y_continuous(limits = c(-20, 20))

