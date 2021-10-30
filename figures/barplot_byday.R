## Purpose of script: This creates the bargraph by days of week plotting the average reports of rape/alcohol offenses
##
## Author: Michael Topper
##
## Date Last Edited: 2021-03-01
##

library(tidyverse)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}


by_day_sexual_assault <- daily_crime %>% 
  mutate(treatment = factor(ifelse(treatment == 1, "Moratorium in Effect", "No Moratorium"), levels = c("No Moratorium", "Moratorium in Effect"))) %>% 
  mutate(day_of_week = factor(day_of_week, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", 'Sat'))) %>% 
  group_by(day_of_week, treatment) %>% 
  summarize(avg_sexual_assault = mean(sexual_assault, na.rm = T),
            avg_alcohol = mean(alcohol_offense, na.rm = T), 
            avg_drug = mean(drug_offense, na.rm = T)) %>% 
  ggplot(aes(x = day_of_week, y = avg_sexual_assault, fill = as.factor(treatment))) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(x = "",y= "Average Reports of Sexual Assault", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

by_day_alcohol <- daily_crime %>% 
  mutate(treatment = factor(ifelse(treatment == 1, "Moratorium in Effect", "No Moratorium"), levels = c("No Moratorium", "Moratorium in Effect"))) %>% 
  mutate(day_of_week = factor(day_of_week, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", 'Sat'))) %>% 
  group_by(day_of_week, treatment) %>% 
  summarize(avg_sexual_assault = mean(sexual_assault, na.rm = T),
            avg_alcohol = mean(alcohol_offense, na.rm = T), 
            avg_drug = mean(drug_offense, na.rm = T)) %>% 
  ggplot(aes(x = day_of_week, y = avg_alcohol, fill = as.factor(treatment))) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(x = "",y= "Average Number of Alcohol Offenses", fill = "") +
  theme_minimal() + 
  theme(legend.position = "bottom")

by_day_drug_offense <- daily_crime %>%
  mutate(treatment = factor(ifelse(treatment == 1, "Moratorium in Place", "No Moratorium"), levels = c("No Moratorium", "Moratorium in Place"))) %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", 'Sat'))) %>%
  group_by(day_of_week, treatment) %>%
  summarize(avg_sexual_assault = mean(sexual_assault, na.rm = T),
            avg_alcohol = mean(alcohol_offense, na.rm = T),
            avg_drug = mean(drug_offense, na.rm = T)) %>%
  ggplot(aes(x = day_of_week, y = avg_drug, fill = as.factor(treatment))) +
  geom_col(position = "dodge",alpha = 0.8) +
  labs(x = "",y= "Average Drug Offense Incidents", fill = "") +
  theme_light() +
  theme(legend.position = "bottom") 

by_day_robbery <- daily_crime %>% 
  mutate(treatment = factor(ifelse(treatment == 1, "Moratorium in Effect", "No Moratorium"), levels = c("No Moratorium", "Moratorium in Effect"))) %>% 
  mutate(day_of_week = factor(day_of_week, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", 'Sat'))) %>% 
  group_by(day_of_week, treatment) %>% 
  summarize(avg_robbery = mean(robbery_burglary,  na.rm = T)) %>% 
  ggplot(aes(x = day_of_week, y = avg_robbery, fill = as.factor(treatment))) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(x = "",y= "Average Number of Robbery/Burglaries", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

# by_day_noise <- daily_crime %>% 
#   mutate(day_of_week = factor(day_of_week, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", 'Sat'))) %>% 
#   group_by(day_of_week, treatment) %>% 
#   summarize(avg_noise = mean(noise_offense,  na.rm = T)) %>% 
#   ggplot(aes(x = day_of_week, y = avg_noise, fill = as.factor(treatment))) +
#   geom_col(position = "dodge") +
#   labs(x = "",y= "", fill = "Fraternity Moratorium") +
#   theme_light()
# 
# by_day_alcohol_strict <- daily_crime %>% 
#   mutate(day_of_week = factor(day_of_week, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", 'Sat'))) %>% 
#   group_by(day_of_week, treatment) %>% 
#   summarize(avg_alcohol = mean(alcohol_offense_strict,  na.rm = T)) %>% 
#   ggplot(aes(x = day_of_week, y = avg_alcohol, fill = as.factor(treatment))) +
#   geom_col(position = "dodge") +
#   labs(x = "",y= "", fill = "Fraternity Moratorium") +
#   theme_light()
# 

by_day_theft <- daily_crime %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", 'Sat'))) %>%
  group_by(day_of_week, treatment) %>%
  summarize(avg_theft = mean(theft,  na.rm = T)) %>%
  ggplot(aes(x = day_of_week, y = avg_theft, fill = as.factor(treatment))) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(x = "",y= "Average Number of Theft Offenses", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom")
