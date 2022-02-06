## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-05
##

library(tidyverse)
library(sf)


boundaries <- st_read("/Users/michaeltopper/birth_shotspotter/data/Boundaries - Police Districts (current)", quiet = T)

rollout_map <- boundaries %>% 
  mutate(dist_num = as.double(dist_num)) %>% 
  mutate(roll_2017 = ifelse(dist_num %in% c(6,7,9,10,11,15), 1, 0),
         roll_2018 = ifelse(dist_num %in% c(2,3,4,5,8,25), 1, 0), 
         roll_2020 = ifelse(dist_num %in% c(16, 17), 1, 0)) %>% 
  mutate(rollout = as.factor(case_when(
    roll_2017 == 1 ~ "2017",
    roll_2018 == 1 ~ "2018",
    roll_2020 == 1 ~ "2020"
  ))) %>% 
  ggplot() +
  geom_sf(aes(fill = rollout), alpha = 0.8) +
  # geom_sf_text(aes(label = rollout)) +
  geom_sf_text(aes(label = dist_num), size = 2) +
  ggthemes::theme_map() +
  theme(legend.key = element_rect(color = NA)) +
  labs(fill = " ", title = "Chicago Police Districts: Rollout of Shotspotter")
