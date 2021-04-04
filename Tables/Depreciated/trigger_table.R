####################################################################################
##  This file creates the table for the counts of reasons why fraternities close  ##
####################################################################################

library(tidyverse)
library(huxtable)
library(glue)

trigger_count <- readxl::read_xlsx("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/trigger_count.xlsx", sheet = "Sheet2")

trigger_figure <- trigger_count %>%
  group_by(category) %>% 
  mutate(total_category = sum(total)) %>% 
  ungroup() %>% 
  mutate(percent = (total_category/sum(total))) %>% 
  mutate(percent = round(percent, 2)) %>% 
  mutate(category = glue("{category} ({percent})")) %>% 
  mutate(category = fct_reorder(category, total)) %>%
  ggplot() +
  geom_col(aes(x = category, y = total, fill = enacted_by)) +
  coord_flip() +
  theme_light()+
  labs(x = "", y = "", fill = "")
  
  