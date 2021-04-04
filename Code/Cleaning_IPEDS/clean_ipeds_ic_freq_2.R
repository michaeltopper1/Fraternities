## Purpose of script: This file cleans the frequently used/ institutional characteristics portion of the ipeds variables
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-19
##

library(tidyverse)


## getting the university names
source("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Code/Cleaning_raw_data_files/clean_excel_closure_dates.R")

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/IPEDS/Frequently_used_instituion_character/Data_2-19-2021---412.csv"

ipeds_ic_freq <- read_csv(path) %>% 
  janitor::clean_names() %>% 
  filter(institution_name %in% closure_table_round$university)

ipeds_ic_freq <- ipeds_ic_freq %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = c(".value", "year"),
               names_sep = "_hd") %>% 
  filter(year >= 2013)

ipeds_ic_freq <- ipeds_ic_freq %>% 
  rename("longitude" = longitude_location_of_institution,
         "latitude" = latitude_location_of_institution) %>% 
  mutate(level_of_institution = case_when(
    level_of_institution == 1 ~ "4-year",
    level_of_institution == 2 ~ "2-year",
    level_of_institution == 3 ~ "less than 2 years"
  )) %>% 
  mutate(control_of_institution = case_when(
    control_of_institution == 1 ~ "Public",
    control_of_institution == 2 ~ "Private not-for-profit",
    control_of_institution == 3 ~ "Private for-profit"
  )) %>% 
  mutate(institutional_category = case_when(
    institutional_category == 1 ~ "Degree-granting, graduate with no undergraduate",
    institutional_category == 2 ~ "Degree-granting, primarily baccalaureate or above"
  )) 

write_csv(ipeds_ic_freq, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/IPEDS/unappended/ipeds_ic_freq_2.csv")
