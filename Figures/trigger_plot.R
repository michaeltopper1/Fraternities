## Purpose of script: creating trigger table
##
## Author: Michael Topper
##
## Date Last Edited: 2021-03-30
##

library(tidyverse)
library(lubridate)
library(glue)

path <- "Created Data/xMaster_data_2021/daily_panel_nosummer.csv"
daily_crime <- read_csv(path, guess_max = 50000)


trigger_table <- daily_crime %>% 
  select(university, starts_with("reason"), starts_with("university_enacted_")) %>% 
  group_by(across(everything())) %>% 
  distinct() %>% 
  ungroup() %>% 
  mutate(across(everything(), ~as.character(.))) %>% 
  pivot_longer(cols = !starts_with("university"), names_to = c("closure_number"), names_pattern = "(\\d)",
               values_to = "reason") %>% 
  filter(!is.na(reason)) %>% 
  mutate(university_enacted = ifelse(university_enacted_1 != university_enacted_2 & closure_number == "2",
                                     university_enacted_2, university_enacted_1)) %>% 
  mutate(university_enacted = ifelse(university_enacted == "1", "University", "IFC")) %>% 
  mutate(reason = str_to_title(reason))

trigger_plot <- trigger_table %>% 
  mutate(reason = fct_lump_min(reason, 3)) %>% 
  count(reason, university_enacted, sort = T) %>% 
  mutate(fraction = round(n/sum(n),2)) %>%
  mutate(reason = fct_reorder(reason, n)) %>% 
  ggplot(aes(reason, n)) +
  geom_col(aes(fill = university_enacted), alpha = 0.8) +
  coord_flip() +
  theme_light() +
  theme(legend.position = "bottom") +
  labs(x = "", y = "", fill = "Source of Enforcement")

