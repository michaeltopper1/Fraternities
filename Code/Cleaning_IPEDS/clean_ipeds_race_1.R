## Purpose of script: Cleans all of the race variables in Ipeds 
## Note that i am using the FALL ENROLLMENT numbers as totals
## Author: Michael Topper
##
## Date Last Edited: 2021-02-19
##

library(tidyverse)

## gets the names of the universities I need
source("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Code/Cleaning_raw_data_files/clean_excel_closure_dates.R")


path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/IPEDS/Fall Enrollment/Data_2-26-2021.csv"
ipeds_fall <- read_csv(path) %>% 
  janitor::clean_names() %>% 
  filter(institution_name %in% closure_table_round$university)

## dividing up these by all students and undergrad totals
ipeds_all_students <- ipeds_fall %>% 
  select(unit_id, institution_name, ends_with("all_students_total"))
ipeds_undergrad <- ipeds_fall %>% 
  select(unit_id, institution_name, ends_with("undergraduate_total"))


## getting undergrad levels
total_men_undergrad <- ipeds_undergrad %>% 
  select(unit_id, institution_name, starts_with("grand_total_men")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_undergrad_men")
total_women_undergrad <- ipeds_undergrad %>% 
  select(unit_id, institution_name, starts_with("grand_total_women")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_undergrad_women")
asian_total_undergrad <- ipeds_undergrad %>% 
  select(unit_id, institution_name, starts_with("asian_total")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_undergrad_asian")
black_total_undergrad <- ipeds_undergrad %>% 
  select(unit_id, institution_name, starts_with("black_or_african_american_total")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_undergrad_black")
hispanic_total_undergrad <- ipeds_undergrad %>% 
  select(unit_id, institution_name, starts_with("hispanic_total")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_undergrad_hispanic")
white_men_total_undergrad <- ipeds_undergrad %>% 
  select(unit_id, institution_name, starts_with("white_men")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_undergrad_white_men")
white_women_total_undergrad <- ipeds_undergrad %>% 
  select(unit_id, institution_name, starts_with("white_women")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_undergrad_white_women")
white_total_undergrad <- ipeds_undergrad %>% 
  select(unit_id, institution_name, starts_with("white_total_ef")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_undergrad_white")




total_men <- ipeds_all_students %>% 
  select(unit_id, institution_name, starts_with("grand_total_men")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_men_all")
total_women <- ipeds_all_students %>% 
  select(unit_id, institution_name, starts_with("grand_total_women")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_women_all")
asian_total <- ipeds_all_students %>% 
  select(unit_id, institution_name, starts_with("asian_total")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_asian_all")
black_total <- ipeds_all_students %>% 
  select(unit_id, institution_name, starts_with("black_or_african_american_total")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_black_all")
white_total <- ipeds_all_students %>% 
  select(unit_id, institution_name, starts_with("white_total")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_white_all")

total_students <- ipeds_all_students %>% 
  select(unit_id, institution_name, starts_with("grand_total_ef")) %>%
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_students_all")

ipeds_race_fall <- total_students %>% 
  left_join(black_total) %>% 
  left_join(asian_total) %>% 
  left_join(total_women) %>% 
  left_join(total_men) %>% 
  left_join(total_men_undergrad) %>% 
  left_join(total_women_undergrad) %>% 
  left_join(black_total_undergrad) %>% 
  left_join(asian_total_undergrad) %>% 
  left_join(hispanic_total_undergrad) %>% 
  left_join(white_men_total_undergrad) %>% 
  left_join(white_women_total_undergrad) %>% 
  left_join(white_total) 


## now grabbing the undergrad totals for full time, part time, and grand totals.
path_2 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/IPEDS/Fall Enrollment/Undergrad_totals.csv"

ipeds_fall_undergrad_totals <- read_csv(path_2) %>% 
  janitor::clean_names() %>% 
  filter(institution_name %in% closure_table_round$university)

total_undergrad_pop <- ipeds_fall_undergrad_totals %>% 
  select(unit_id, institution_name, starts_with("grand_total_")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "total_students_undergrad")

full_time_undergrad <- ipeds_fall_undergrad_totals %>% 
  select(unit_id, institution_name, starts_with("full_time_total")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "ftime_total_undergrad")

full_time_undergrad_men <- ipeds_fall_undergrad_totals %>% 
  select(unit_id, institution_name, starts_with("full_time_men")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "ftime_total_undergrad_men")

full_time_undergrad_women <- ipeds_fall_undergrad_totals %>% 
  select(unit_id, institution_name, starts_with("full_time_women")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = "year",
               names_pattern = "(\\d\\d\\d\\d)", values_to = "ftime_total_undergrad_women")


ipeds_race_fall <- ipeds_race_fall %>% 
  left_join(full_time_undergrad) %>% 
  left_join(full_time_undergrad_men) %>% 
  left_join(full_time_undergrad_women) %>% 
  left_join(total_undergrad_pop)


write_csv(ipeds_race_fall, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/IPEDS/unappended/ipeds_race_1.csv")
