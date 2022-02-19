## Purpose of script: get the group b offenses to correct panel format
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-17
##

library(tidyverse)

group_b <- read_csv("created_data/nibrs/group_b_arrests.csv")
nibrs_all <- read_csv("created_data/nibrs/final_panel_all.csv")

group_b <- group_b %>% 
  mutate(alcohol_arrest = ifelse(ucr_arrest_offense_code == "driving under the influence" |
                                    ucr_arrest_offense_code == "drunkenness" |
                                    ucr_arrest_offense_code == "liquor law violations", 1, 0)) %>% 
  mutate(college_aged = ifelse(age_of_arrestee >=17 & age_of_arrestee <= 22, "college_aged", "not_college")) %>% 
  group_by(arrest_date, ori, college_aged) %>% 
  summarize(alcohol_arrest = sum(alcohol_arrest,na.rm = T)) %>% ungroup() %>% 
  pivot_wider( names_from = college_aged, values_from = alcohol_arrest) %>% 
  select( -`NA`) %>% 
  rename(alcohol_arrest_college_aged = college_aged, alcohol_arrest_not_college_aged = not_college) %>% 
  mutate(across(-c(arrest_date, ori), ~ifelse(is.na(.), 0, .)))


group_b %>% 
  rowwise() %>% 
  mutate(alcohol_offense_total = sum(alcohol_arrest_college_aged, alcohol_arrest_not_college_aged, na.rm = T)) %>% 
  ggplot(aes(arrest_date, alcohol_offense_total)) +
  geom_path() +
  facet_wrap(~ori,scales = "free_y")
group_b %>% distinct(ori) %>% pull()
nibrs_all %>% distinct(ori_9) %>% pull()

nibrs_all %>% 
  left_join(group_b, by = c("date" = "arrest_date", "ori"))
