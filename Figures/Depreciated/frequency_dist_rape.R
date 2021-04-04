################################################################################
##  This file creates the frequency distribution graph for rape (not logged)  ##
################################################################################

library(tidyverse)

load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Master_data/master_ucr_ipeds_cross.rda")

colnames(ucr_master)

ucr_master <- ucr_master %>% 
  mutate(actual_rape_total = ifelse(actual_rape_total == -1, NA, actual_rape_total)) %>% 
  mutate(log_rape = log(actual_rape_total + 1))

frequency_dist_rape <- ucr_master %>% 
  count(actual_rape_total, sort = T) %>% 
  mutate(actual_rape_total = fct_reorder(as.factor(actual_rape_total), desc(n))) %>% 
  head(10) %>% 
  ggplot() +
  geom_col(aes(x = actual_rape_total, y = n)) +
  theme_light() +
  labs(y = "", x = "")

frequency_dist_rape

