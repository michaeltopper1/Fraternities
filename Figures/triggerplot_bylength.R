## Purpose of script: creates the new trigger plot
##
## Author: Michael Topper
##
## Date Last Edited: 2021-04-15
##

library(tidyverse)

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv", guess_max = 50000)


length_1 <- daily_crime %>% 
  mutate(length_1 = closure_1_end - closure_1, length_2 = closure_2_end - closure_2) %>% 
  distinct(length_1, university, university_enacted_1, reason1) %>% 
  rename("university_enacted" = "university_enacted_1", "length" = "length_1", "reason" = "reason1")
length_2 <- daily_crime %>% 
  mutate(length_1 = closure_1_end - closure_1, length_2 = closure_2_end - closure_2) %>% 
  distinct(length_2, university, university_enacted_2, reason2) %>% 
  filter(!is.na(university_enacted_2)) %>% 
  rename("university_enacted" = "university_enacted_2", "length" = "length_2", "reason" = "reason2")

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
  extract(length, "length", "(\\d{1,})") %>% 
  mutate(length = as.double(length)) %>% 
  group_by(reason) %>% 
  mutate(avg_length = round(mean(length, na.rm = T), 1)) %>% 
  add_count(reason) %>% 
  mutate(reason = glue("{reason} ({avg_length} days on average)")) %>% 
  mutate(university_enacted = ifelse(university_enacted == 1, "University Enacted", "IFC Enacted")) 

trigger_plot <- length_graph %>% 
  mutate(university = fct_reorder(university, length )) %>% 
  ggplot(aes(university, length, fill = factor(university_enacted))) +
  geom_col() + coord_flip() +
  facet_wrap(~reason) + theme_light() +
  labs(y = "Length of Moratorium in Days", x= "", fill = "") +
  theme(legend.position ="bottom", strip.text.x = element_text(size = 14), strip.text = element_text(colour = 'white'),
        strip.background = element_rect(fill = "gray60"), legend.key.size = unit(.5, "cm"))
