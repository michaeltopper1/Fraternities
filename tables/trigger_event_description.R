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
  select(-reason_2, -total_closures, - citation) %>% 
  mutate(university = ifelse(university == "Louisiana State University and Agricultural & Mechanical College", "Louisiana State University", university)) %>% 
  mutate(university = ifelse(university == "California Polytechnic State University-San Luis Obispo", "Cal Poly San Luis Obispo", university)) %>%
  mutate(university = ifelse(university == "University of Pittsburgh-Pittsburgh Campus", "University of Pittsburgh", university)) 

reasons_table <- reasons %>% 
  mutate(across(everything(), ~as.character(.))) %>% 
  mutate(across(everything(), ~ifelse(is.na(.), " ", .))) %>% 
  kbl(col.names = c("University", "Description of Triggering Event", "Triggering Event Date", "Moratorium Start Date", "Classification"
  ), booktabs = T, longtable = T,
  caption = "\\label{reasons_table}Description of the Triggering Events that lead to a Moratorium") %>% 
  footnote(list("Description of the triggering event is summarized based on newsarticles or conversations with Fraternity and Sorority Life staff. The date of the triggering event is shown if provided. The classification of each event is based off of the description and aligns with Figure \ref{triggerplot}.")
               ,threeparttable = T)
