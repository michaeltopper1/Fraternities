
## This file creates the list of schools and their respective ORIS
library(tidyverse)

load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Master_data/master_ucr_ipeds_cross.rda")

orischools_table <- ucr_master %>% 
  group_by(university, name, ori9, fplace) %>% 
  count() %>% 
  arrange(university) %>% 
  select("University" = university, "Local Municipality" = name, "ORI" = ori9, "Fplace Code" = fplace) 





