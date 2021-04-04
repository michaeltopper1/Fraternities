## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-24
##

library(tidyverse)

clery_act_sex <- read_csv("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Clery_act_data/sexual_assault_panel_final.csv")
clery_act_discipline <- read_csv("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Clery_act_data/drug_alcohol_panel_final.csv")

clery_act <- clery_act_sex %>% 
  left_join(clery_act_discipline)

write_csv(clery_act, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Clery_act_data/xxclery_act_final.csv")
