## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-08
##

library(tidyverse)

ipeds <- read_csv("Data/IPEDS/ipeds_2_5-8-21.csv")

## getting colnames into a tibble for easy manipulation
ipeds_names <- colnames(ipeds) %>% 
  as_tibble() %>% 
  rename("colnames" = value) %>% 
  extract(colnames, "year_place", "(\\(.{1,}\\))", remove = F) %>% 
  extract(year_place, "year", "(\\d{4})", remove = F) %>% 
  mutate(colnames = str_replace(colnames, "\\(.{1,}\\)$", "")) 


ipeds_names <- ipeds_names %>% 
  mutate(colnames = str_trim(colnames)) %>% 
  mutate(colnames = case_when(
    colnames == "SAT Evidence-Based Reading and Writing 25th percentile score" ~ "sat_reading_25",
    colnames == "SAT Evidence-Based Reading and Writing 75th percentile score" ~ "sat_reading_75",
    colnames == "SAT Math 25th percentile score" ~ "sat_math_25",
    colnames == "SAT Math 75th percentile score" ~ "sat_math_75",
    colnames == "Latitude location of institution" ~ "latitude",
    colnames == "Longitude location of institution" ~ "longitude",
    colnames == "SAT Critical Reading 75th percentile score" ~"sat_reading_75",
    colnames == "SAT Critical Reading 25th percentile score" ~"sat_reading_25",
    TRUE ~as.character(colnames)
  )) 

ipeds_names <- ipeds_names %>% 
  mutate(colnames = paste0(colnames, " ", year)) %>% 
  mutate(colnames = str_trim(str_replace(colnames, "NA$", "")))

cleaned_colnames <- ipeds_names$colnames

colnames(ipeds) <- cleaned_colnames

ipeds_long_2 <- ipeds %>% 
  janitor::clean_names() %>% 
  select(-x51) %>% 
  rename("university" = institution_name) %>% 
  pivot_longer(cols = -c(unit_id, university), names_to = c(".value", "year"), names_pattern = "(.{1,})_(\\d{4})") 


write_csv(ipeds_long_2, file = "Created Data/IPEDS/ipeds_2_cleaned.csv")
