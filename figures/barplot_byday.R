## Purpose of script: This creates the bargraph by days of week plotting the average reports of rape/alcohol offenses
##
## Author: Michael Topper
##
## Date Last Edited: 2021-03-01
##

library(tidyverse)
library(patchwork)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}

median_alc_freq <- daily_crime %>% 
  group_by(university) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  distinct(university, frequency_alc) %>% 
  arrange(desc(frequency_alc)) %>% 
  pull(frequency_alc) %>% 
  quantile(c(0.5))

median_sex_freq <- daily_crime %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  distinct(university, frequency_sex) %>% 
  pull(frequency_sex) %>% 
  quantile(c(0.5))

sex_bar <- daily_crime %>% 
  group_by(university) %>% 
  mutate(frequency_sex = sum(sexual_assault_per25, na.rm = T)) %>% 
  mutate(over_median_sex = ifelse(frequency_sex > median_sex_freq, "Above Median Frequency", "Below Median Frequency")) %>% 
  ungroup() %>% 
  mutate(treatment = ifelse(treatment == 1, "Moratorium", "No Moratorium")) %>% 
  mutate(weekend = ifelse(day_of_week %in% c("Fri", "Sat", "Sun"), "Weekends", "Weekdays")) %>% 
  group_by(treatment, over_median_sex, weekend) %>% 
  summarize(avg_sex = mean(sexual_assault_per25, na.rm = T)) %>% 
  # mutate(day_of_week = factor(day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>% 
  ggplot(aes(weekend, avg_sex)) +
  geom_col(aes(fill = as.factor(treatment)), position = "dodge")  +
  facet_wrap(~over_median_sex) +
  labs(x = " ", y = "Average Per 25000 Students", fill = " ", title = "Panel B: Sexual Assaults") +
  theme_minimal() +
  theme(legend.position = "bottom")

alc_bar <- daily_crime %>% 
  group_by(university) %>% 
  mutate(frequency_alc= sum(alcohol_offense_per25, na.rm = T)) %>% 
  mutate(over_median_alc = ifelse(frequency_alc > median_alc_freq, "Above Median Frequency", "Below Median Frequency")) %>%
  ungroup() %>% 
  mutate(treatment = ifelse(treatment == 1, "Moratorium", "No Moratorium")) %>% 
  mutate(weekend = ifelse(day_of_week %in% c("Fri", "Sat", "Sun"), "Weekends", "Weekdays")) %>% 
  group_by(treatment, over_median_alc, weekend) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T)) %>% 
  # mutate(day_of_week = factor(day_of_week, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))) %>% 
  ggplot(aes(weekend, avg_alc)) +
  geom_col(aes(fill = as.factor(treatment)), position = "dodge")  +
  facet_wrap(~over_median_alc) +
  labs(x = " ", y = "Average Per 25000 Students", fill = " ", title = "Panel A: Alcohol Offenses") +
  theme_minimal() +
  theme(legend.position = "none")

raw_bar <- alc_bar + sex_bar +plot_layout(ncol = 1)

