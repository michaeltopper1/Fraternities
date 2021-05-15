## Purpose of script: creates the new trigger plot
##
## Author: Michael Topper
##
## Date Last Edited: 2021-04-15
##

library(tidyverse)
library(glue)
library(tidytext)
daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv", guess_max = 50000)


length_1 <- daily_crime %>% 
  group_by(university) %>% 
  filter(date >= closure_1 & date < closure_1_end) %>% 
  count(treatment, reason1, university_enacted_1) %>% 
  rename("university_enacted" = "university_enacted_1", "length" = "n", "reason" = "reason1")
length_2 <- daily_crime %>% 
  group_by(university) %>% 
  filter(date >= closure_2 & date < closure_2_end) %>% 
  count(treatment, reason2, university_enacted_2) %>% 
  rename("university_enacted" = "university_enacted_2", "length" = "n", "reason" = "reason2")

length <- bind_rows(length_1, length_2)


## note: behavior contains conduct violations/bad bheavior/not following rules/racist activity/alcohol violations/hazing
## note: unknown contains "national trends/
length <- length %>% 
  mutate(reason = ifelse(reason == "bad behavior", "behavior", reason),
         reason = ifelse(reason == "conduct violation", "behavior", reason ),
         reason = ifelse(reason == "not following rules", "behavior", reason),
         reason = ifelse(reason == "racist", "racist activity", reason),
         reason = ifelse(reason == "trends", "national trends", reason), 
         reason = ifelse(reason == "other", "unknown", reason),
         reason = ifelse(reason == "alcohol", "behavior", reason),
         reason = ifelse(reason == "hazing", "behavior", reason),
         reason = ifelse(reason == "national trends", "unknown", reason),
         reason = ifelse(reason == "racist activity", "behavior", reason)) %>% 
  mutate(reason = str_to_title(reason))

length_graph <- length %>% 
  group_by(reason) %>% 
  mutate(avg_length = round(mean(length, na.rm = T), 1)) %>% 
  add_count(reason) %>% 
  mutate(reason = glue("{reason} ({avg_length} days on average)")) %>% 
  mutate(university_enacted = ifelse(university_enacted == 1, "University Enacted", "IFC Enacted")) 

trigger_plot <- length_graph %>% 
  mutate(university = ifelse(university == "Louisiana State University and Agricultural & Mechanical College", "Louisiana State University", university)) %>% 
  mutate(university = ifelse(university == "California Polytechnic State University-San Luis Obispo", "Cal Poly San Luis Obispo", university)) %>%
  mutate(university = ifelse(university == "University of Pittsburgh-Pittsburgh Campus", "University of Pittsburgh", university)) %>%
  mutate(university = gsub("-Main Campus", "", university)) %>% 
  mutate(university = ifelse(university == "North Carolina State University at Raleigh", "North Carolina State", university)) %>% 
  mutate(university = reorder_within(university, length, reason)) %>% 
  filter(!is.na(length)) %>% 
  ggplot(aes(university, length, fill = factor(university_enacted))) +
  geom_col(alpha = 0.8) + coord_flip() +
  geom_text(aes(label = length), color = "black", size = 3, hjust = -.1) +
  facet_wrap(~reason, scales = "free_y") +
  scale_x_reordered() +
  ggthemes::theme_clean() +
  labs(y = "Length of Moratorium in Days", x= "", fill = "", caption = "Note: Behavior contains conduct violations/racist activity/alcohol violations/hazing") +
  theme(legend.position ="bottom", strip.text = element_text(colour = 'white'),
        strip.background = element_rect(fill = "gray60"), legend.key.size = unit(.5, "cm"))


