## Purpose of script: This file merges all the clery act data with the yearly crime logs.
##
## Author: Michael Topper
##
## Date Last Edited: 2021-09-28
##

library(tidyverse)
library(lubridate)
library(fixest)


# loading in data ---------------------------------------------------------


yearly_crime <- read_csv("created_data/xmaster_data/yearly_panel_full_calendar.csv") %>% 
  filter(year > 2013)

clery_discipline <- read_csv("created_data/clery_act/discipline.csv")

clery_crime <- read_csv("created_data/clery_act/crime.csv")

closure_spreadsheet <- readxl::read_excel("data/closure_spreadsheet_final_2019.xlsx") %>% 
  janitor::clean_names() %>% 
  select(university, starts_with("reason"), starts_with("university_enacted"))

# merging all together ----------------------------------------------------

all_crime <- yearly_crime %>% 
  left_join(clery_discipline) %>% 
  left_join(clery_crime) %>% 
  left_join(closure_spreadsheet)



# creating totals for clery act crimes so that i can compare with  --------

all_crime <- all_crime %>% 
  mutate(clery_sexual_assault = noncampus_rape + noncampus_fondl  +
            oncampus_rape + oncampus_fondl  + publicproperty_rape + 
           publicproperty_fondl,
         clery_alcohol = noncampus_liquor + oncampus_liquor + publicproperty_liquor,
         clery_drug = noncampus_drug + oncampus_drug + publicproperty_drug,
         clery_robbery = noncampus_robbe + oncampus_robbe + publicproperty_robbe) %>% 
  mutate(clery_offcampus_sexual_assault = noncampus_rape + noncampus_fondl + noncampus_inces +
           publicproperty_rape + publicproperty_fondl + publicproperty_inces) %>% 
  mutate(clery_oncampus_sexual_assault = oncampus_rape + oncampus_fondl) %>% 
  mutate(clery_oncampus_drug = oncampus_drug) %>% 
  mutate(clery_oncampus_liquor = oncampus_liquor) %>% 
  mutate(residencehall_sexual_assault = residencehall_rape + residencehall_fondl) %>% 
  mutate(across(starts_with("clery_"), ~ (./total_enrollment) * 25000, .names = "{.col}_per25")) %>% 
  mutate(across(c(residencehall_sexual_assault, residencehall_drug, residencehall_liquor), ~ (./total_enrollment) * 25000, .names = "{.col}_per25")) %>% 
  mutate(across(c(sexual_assault, alcohol_offense, robbery_burglary, drug_offense), ~ (./total_enrollment) * 25000, .names = "{.col}_per25")) %>% 
  mutate(across(c(noncampus_liquor, noncampus_drug, noncampus_rape), ~(./total_enrollment) * 25000, .names = "{.col}_per25")) %>% 
  mutate(across(clery_offcampus_sexual_assault, ~(./total_enrollment) * 25000, .names = "{.col}_per25"))
  



# changing to groupings of reasons ----------------------------------------

all_crime <- all_crime %>% 
  mutate(reason1 = ifelse(reason1 == "bad behavior", "behavior", reason1),
         reason1 = ifelse(reason1 == "conduct violation", "behavior", reason1 ),
         reason1 = ifelse(reason1 == "not following rules", "behavior", reason1),
         reason1 = ifelse(reason1 == "racist", "racist activity", reason1),
         reason1 = ifelse(reason1 == "trends", "national trends", reason1), 
         reason1 = ifelse(reason1 == "other", "unknown", reason1),
         reason1 = ifelse(reason1 == "alcohol", "behavior", reason1),
         reason1 = ifelse(reason1 == "hazing", "behavior", reason1),
         reason1 = ifelse(reason1 == "national trends", "unknown", reason1),
         reason1 = ifelse(reason1 == "racist activity", "behavior", reason1)) %>% 
  mutate(reason2 = ifelse(reason2 == "bad behavior", "behavior", reason2),
         reason2 = ifelse(reason2 == "conduct violation", "behavior", reason2 ),
         reason2 = ifelse(reason2 == "not following rules", "behavior", reason2),
         reason2 = ifelse(reason2 == "racist", "racist activity", reason2),
         reason2 = ifelse(reason2 == "trends", "national trends", reason2), 
         reason2 = ifelse(reason2 == "other", "unknown", reason2),
         reason2 = ifelse(reason2 == "alcohol", "behavior", reason2),
         reason2 = ifelse(reason2 == "hazing", "behavior", reason2),
         reason2 = ifelse(reason2 == "national trends", "unknown", reason2),
         reason2 = ifelse(reason2 == "racist activity", "behavior", reason2))

all_crime <- all_crime %>% 
  filter(university %in% ifc::moratorium_schools())


write_csv(all_crime, file = "created_data/xmaster_data/merged_clery.csv")

