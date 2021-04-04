#################################################################
##   This study runs the event studies for logs and non logs   ##
#################################################################

library(tidyverse)
library(lfe)
library(broom)
library(modelsummary)
library(kableExtra)

load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Master_data/event_study.rda")
event_study <- event_study %>% 
  filter(university != "California State University-Northridge"  & university != "Miami University-Oxford") %>% 
  filter(subtype2 == "(011) Four-year university")

event_study_inverse <- felm(inverse_sine_rape  ~ treatment_minus_8 + 
                      treatment_minus_7 + treatment_minus_6 + 
                      treatment_minus_5 + treatment_minus_4 + 
                      treatment_minus_3 + treatment_minus_2 + 
                      treatment + treatment_plus_1 +
                      treatment_plus_2  | university + month_by_year | 0 | university, data = event_study)

event_study_percap <- felm(rape_per_hundredthousand  ~ treatment_minus_8 + 
                      treatment_minus_7 + treatment_minus_6 + 
                      treatment_minus_5 + treatment_minus_4 + 
                      treatment_minus_3 + treatment_minus_2 + 
                      treatment + treatment_plus_1 + 
                      treatment_plus_2| university + month_by_year  | 0 | university, data = event_study)

coefs_inverse <- tidy(event_study_inverse, conf.int = T)
coefs_percap <- tidy(event_study_percap, conf.int = T)
coefs_inverse <- coefs_inverse %>% 
  add_row(.before = 8)
coefs_percap <- coefs_percap %>% 
  add_row(.before = 8)

event_window <- c(-8:2)
event_window <- as_tibble(event_window)


coefs_inverse <- tibble(coefs_inverse, event_window)
coefs_percap <- tibble(coefs_percap, event_window)

coefs_inverse <-  coefs_inverse %>% 
  mutate(estimate = ifelse(is.na(estimate), 0, estimate))

coefs_percap <- coefs_percap %>% 
  mutate(estimate = ifelse(is.na(estimate), 0, estimate))


coefs_inverse_plot <- coefs_inverse %>% 
  ggplot(aes(x = value, y = estimate), alpha = 0.8) +
  geom_path(linetype = "dashed", alpha = 0.8) +
  geom_point() +
  geom_errorbar(aes(x = value, ymin = conf.low, ymax = conf.high), alpha = 0.8) +
  geom_rect(aes(xmin = -0.5, xmax =0.5, ymin = -Inf, ymax = Inf), alpha = 0.03)  +
  scale_x_continuous(labels = c(-8:2), breaks = c(-8:2)) +
  scale_y_continuous(limits = c(-0.8,0.8))+
  labs(x = "", y = "") +
  ggthemes::theme_clean()

coefs_percap_plot <- coefs_percap %>% 
  ggplot(aes(x = value, y = estimate), alpha = 0.8) +
  geom_path(linetype = "dashed", alpha = 0.8) +
  geom_point() +
  geom_errorbar(aes(x = value, ymin = conf.low, ymax = conf.high), alpha = 0.8) +
  geom_rect(aes(xmin = -0.5, xmax =0.5, ymin = -Inf, ymax = Inf), alpha = 0.03)  +
  scale_x_continuous(labels = c(-8:2), breaks = c(-8:2)) +
  scale_y_continuous(limits = c(-6,6)) +
  labs(x = "", y = "") +
  ggthemes::theme_clean()


event_studies <- list("Per-capita Rape" = coefs_percap_plot, "Inverse Hyperbolic Sine Rape" = coefs_inverse_plot)

