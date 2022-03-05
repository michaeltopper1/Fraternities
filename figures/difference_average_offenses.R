## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-03-01
##

library(tidyverse)
library(kableExtra)
library(tidytext)
library(patchwork)

if (!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}


alc_bar <- daily_crime %>% 
  mutate(weekend = ifelse(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun", "Weekends", "Weekdays")) %>% 
  mutate(weekend = factor(weekend, levels = c("Weekends", "Weekdays"))) %>% 
  mutate(treatment = ifelse(treatment == 1, "moratorium", "not")) %>% 
  group_by(university, treatment, weekend) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T),
            avg_sex = mean(sexual_assault_per25, na.rm = T)) %>% 
  pivot_wider( names_from = treatment, values_from = c(avg_alc, avg_sex)) %>% 
  rowwise() %>% 
  mutate(alc_difference = avg_alc_moratorium -avg_alc_not,
         sex_difference = avg_sex_moratorium - avg_sex_not) %>% 
  ungroup() %>%
  mutate(positive = ifelse(alc_difference >=0, "Average Offenses Higher in Moratorium", "Average Offenses Lower in Moratorium")) %>% 
  mutate(university = reorder_within(university, alc_difference, weekend)) %>%
  ggplot(aes(university, alc_difference, fill = positive)) +
  geom_col() +
  facet_wrap(~weekend, scales = "free_x") +
  scale_x_reordered() +
  labs(x = "Universities",y = "Difference in Average Offenses", fill = " ", title = "Panel A: Alcohol Offenses") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_blank())

sex_bar <- daily_crime %>% 
  mutate(weekend = ifelse(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun", "Weekends", "Weekdays")) %>% 
  mutate(weekend = factor(weekend, levels = c("Weekends", "Weekdays"))) %>% 
  mutate(treatment = ifelse(treatment == 1, "moratorium", "not")) %>% 
  group_by(university, treatment, weekend) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T),
            avg_sex = mean(sexual_assault_per25, na.rm = T)) %>% 
  pivot_wider( names_from = treatment, values_from = c(avg_alc, avg_sex)) %>% 
  rowwise() %>% 
  mutate(alc_difference = avg_alc_moratorium -avg_alc_not,
         sex_difference = avg_sex_moratorium - avg_sex_not) %>% 
  ungroup() %>%
  mutate(university = reorder_within(university, sex_difference, weekend)) %>%
  mutate(positive = ifelse(sex_difference >=0, "Average Offenses Higher in Moratorium", "Average Offenses Lower in Moratorium")) %>% 
  ggplot(aes(university, sex_difference, fill = positive)) +
  geom_col() +
  facet_wrap(~weekend, scales = "free_x") +
  scale_x_reordered() +
  labs(x = "Universities",y = "Difference in Average Offenses", fill = " ", title = "Panel B: Sexual Assaults") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_blank())

diff_average_offenses <- alc_bar + sex_bar + plot_layout(nrow = 2)
