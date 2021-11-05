## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-29
##

library(tidyverse)

semester_crime <- read_csv("created_data/xmaster_data/semester_level.csv")


barplot_weekends <- semester_level %>% 
  mutate(university = ifelse(university == "Louisiana State University and Agricultural & Mechanical College", "Louisiana State University", university)) %>% 
  mutate(university = ifelse(university == "California Polytechnic State University-San Luis Obispo", "Cal Poly San Luis Obispo", university)) %>%
  mutate(university = ifelse(university == "University of Pittsburgh-Pittsburgh Campus", "University of Pittsburgh", university)) %>%
  mutate(university = gsub("-Main Campus", "", university)) %>% 
  mutate(university = ifelse(university == "North Carolina State University at Raleigh", "North Carolina State", university)) %>% 
  mutate(treatment = ifelse(treatment >0, "Moratorium", "No Moratorium")) %>% 
  group_by(university, treatment) %>% 
  summarize(across(c(alcohol_offense_per25, drug_offense_per25, sexual_assault_per25), ~mean(.,na.rm =T))) %>% 
  pivot_longer(cols = 3:5, names_to = "offense", values_to = "average") %>% 
  mutate(moratorium = ifelse(treatment =="Moratorium", 1, 0)) %>% 
  mutate(offense = case_when(
    offense == "drug_offense_per25" ~"Drug",
    offense == "alcohol_offense_per25" ~"Alcohol",
    offense == "sexual_assault_per25" ~"Sex"
  )) %>% 
  ggplot(aes(offense, average, fill = moratorium)) +
  geom_col(position = "dodge2") +
  facet_wrap(~university, scales = "free_y") +
  labs(y = "Average Offenses") +
  theme_minimal() 

semester_level %>% 
  mutate(university = ifelse(university == "Louisiana State University and Agricultural & Mechanical College", "Louisiana State University", university)) %>% 
  mutate(university = ifelse(university == "California Polytechnic State University-San Luis Obispo", "Cal Poly San Luis Obispo", university)) %>%
  mutate(university = ifelse(university == "University of Pittsburgh-Pittsburgh Campus", "University of Pittsburgh", university)) %>%
  mutate(university = gsub("-Main Campus", "", university)) %>% 
  mutate(university = ifelse(university == "North Carolina State University at Raleigh", "North Carolina State", university)) %>% 
  mutate(moratorium = case_when(
    treatment > 0 ~ "Moratorium",
    semester_before_dose_indicator > 0 ~"Semester Before",
    semester_after_dose_indicator > 0 ~ "Semester After",
    TRUE ~ as.character("None")
  )) %>% 
  group_by(university, moratorium) %>% 
  summarize(across(c(alcohol_offense_per25, drug_offense_per25, sexual_assault_per25), ~mean(.,na.rm =T))) %>% 
  mutate(on_moratorium = ifelse(moratorium == "Moratorium", 1, 0)) %>% 
  ggplot(aes(moratorium, alcohol_offense_per25, fill = on_moratorium)) +
  geom_col(position = "dodge2") +
  facet_wrap(~university, scales = "free_y") +
  labs(y = "Average Offenses") +
  theme_minimal() 
  

barplot_full <- daily_crime %>% 
  mutate(university = ifelse(university == "Louisiana State University and Agricultural & Mechanical College", "Louisiana State University", university)) %>% 
  mutate(university = ifelse(university == "California Polytechnic State University-San Luis Obispo", "Cal Poly San Luis Obispo", university)) %>%
  mutate(university = ifelse(university == "University of Pittsburgh-Pittsburgh Campus", "University of Pittsburgh", university)) %>%
  mutate(university = gsub("-Main Campus", "", university)) %>% 
  mutate(university = ifelse(university == "North Carolina State University at Raleigh", "North Carolina State", university)) %>% 
  # filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun") %>% 
  mutate(treatment = ifelse(treatment == 1, "Moratorium", "No Moratorium")) %>% 
  group_by(university, treatment) %>% 
  summarize(across(c(alcohol_offense_per25, drug_offense_per25, sexual_assault_per25), ~mean(.,na.rm =T))) %>% 
  pivot_longer(cols = 3:5, names_to = "offense", values_to = "average") %>% 
  mutate(moratorium = ifelse(treatment =="Moratorium", 1, 0)) %>% 
  mutate(offense = case_when(
    offense == "drug_offense_per25" ~"Drug",
    offense == "alcohol_offense_per25" ~"Alcohol",
    offense == "sexual_assault_per25" ~"Sex"
  )) %>% 
  ggplot(aes(offense, average, fill = moratorium)) +
  geom_col(position = "dodge2") +
  facet_wrap(~university, scales = "free_y") +
  labs(y = "Average Offenses") +
  theme_minimal() 

