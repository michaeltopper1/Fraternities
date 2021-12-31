## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-12-24
##

library(tidyverse)
library(kableExtra)
reasons <- readxl::read_excel("data/closure_reasons.xlsx") %>% 
  janitor::clean_names() %>% 
  select(-reason_2, -total_closures, - citation)

reasons_table <- reasons %>% 
  mutate(across(everything(), ~as.character(.))) %>% 
  mutate(across(everything(), ~ifelse(is.na(.), " ", .))) %>% 
  kbl(col.names = c("University", "Description of Triggering Event", "Triggering Event Date", "Moratorium Start Date", "Classification"
  ), booktabs = T, 
  caption = "\\label{reasons_table}Description of the Triggering Events that lead to a Moratorium") %>% 
  kable_styling()
