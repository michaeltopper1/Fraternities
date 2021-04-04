## Purpose of script: Get the SAT math score out of the ipeds data
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-19
##

library(tidyverse)


## getting the university names
source("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Code/Cleaning_raw_data_files/clean_excel_closure_dates.R")

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/IPEDS/SAT/Data_2-19-2021.csv"

ipeds_sat <- read_csv(path) %>% 
  janitor::clean_names() %>% 
  filter(institution_name %in% closure_table_round$university)

ipeds_sat_math <- ipeds_sat %>% 
  select(unit_id, institution_name, starts_with("sat_math_75")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = c(".value", "year"),
               names_sep = "adm|ic") %>% 
  extract(year, "year", "(\\d\\d\\d\\d)") %>% 
  mutate(year = as.double(year) ) %>% 
  filter(year >= 2013)
  

write_csv(ipeds_sat_math, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/IPEDS/unappended/ipeds_sat_4.csv")
