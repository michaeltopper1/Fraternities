## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-06
##

library(tidyverse)
library(lubridate)

admin <- read_csv("Created Data/nibrs/admin_data.csv")
admin <- admin %>% 
  group_by(ori, incident_number) %>% 
  distinct() %>% 
  ungroup()

offenses <- read_csv("Created Data/nibrs/offense_data.csv", guess_max = 50000)

offenses <- offenses %>% 
  group_by(ori, incident_number) %>% 
  distinct() %>% 
  ungroup()

victims <- read_csv("Created Data/nibrs/victim_data.csv", guess_max = 50000)

victims <- victims %>% 
  group_by(ori, incident_number) %>% 
  distinct() %>% 
  ungroup()


nibrs <- offenses %>% 
  left_join(admin) %>% 
  left_join(victims)

closures <- readxl::read_excel("Data/closure_spreadsheet_final_2019.xlsx") %>% janitor::clean_names()


# closures <- closures %>% 
#   pivot_longer(cols = matches("^ori"), values_to = "ori", names_to = "source") %>% 
#   relocate(ori, source)

## start with only sexual assaults and see if you get anything interesting
## can move on to burglaries/robberies later if you really think there's a point.

nibrs <- nibrs %>% 
  mutate(rape = ifelse(ucr_offense_code == "rape" | 
                         ucr_offense_code_1 == "rape"|
                         ucr_offense_code_2 =="rape" |
                         ucr_offense_code_3 == "rape"|
                         ucr_offense_code_4 == "rape", 1, 0)) %>% 
  mutate(sexual_assault_object = ifelse(ucr_offense_code == "sexual assault with an object" | 
                                          ucr_offense_code_1 == "sexual assault with an object"|
                                          ucr_offense_code_2 =="sexual assault with an object" |
                                          ucr_offense_code_3 == "sexual assault with an object"|
                                          ucr_offense_code_4 == "sexual assault with an object", 1, 0)) %>%
  mutate(fondling = ifelse(ucr_offense_code == "fondling (incident liberties/child molest)" | 
                                          ucr_offense_code_1 == "fondling (incident liberties/child molest)"|
                                          ucr_offense_code_2 =="fondling (incident liberties/child molest)" |
                                          ucr_offense_code_3 == "fondling (incident liberties/child molest)"|
                                          ucr_offense_code_4 == "fondling (incident liberties/child molest)", 1, 0)) %>% 
  mutate(rape_statutory = ifelse(ucr_offense_code == "statutory rape" | 
                         ucr_offense_code_1 == "statutory rape"|
                         ucr_offense_code_2 =="statutory rape" |
                         ucr_offense_code_3 == "statutory rape"|
                         ucr_offense_code_4 == "statutory rape", 1, 0)) %>% 
  mutate(theft = ifelse(str_detect(ucr_offense_code, "theft") | 
                          str_detect(ucr_offense_code_1, "theft")|
                          str_detect(ucr_offense_code_2, "theft")|
                          str_detect(ucr_offense_code_3, "theft")|
                          str_detect(ucr_offense_code_4, "theft"), 1, 0)) 



nibrs <- nibrs %>% 
  mutate(age_of_victim = as.double(age_of_victim)) %>% 
  mutate(college_age_rape = ifelse(age_of_victim >= 17 & age_of_victim <=24 & (ucr_offense_code == "rape"), 1, 0)) %>%
  mutate(college_age_sexual_assault_object = ifelse(age_of_victim >= 17 & age_of_victim <=24 & (ucr_offense_code == "sexual assault with an object"), 1, 0)) %>% 
  mutate(college_age_fondle = ifelse(age_of_victim >= 17 & age_of_victim <=24 & (ucr_offense_code == "fondling (incident liberties/child molest)"), 1, 0)) %>%
  mutate(college_age_rape_statutory = ifelse(age_of_victim >= 17 & age_of_victim <=24 & (ucr_offense_code == "statutory rape"), 1, 0)) %>%
  mutate(victim_female = ifelse(sex_of_victim == "female", 1, 0)) %>% 
  mutate(victim_black = ifelse(race_of_victim == "black", 1, 0)) %>% 
  mutate(victim_asian = ifelse(race_of_victim == "asian", 1, 0)) %>% 
  mutate(victim_white = ifelse(race_of_victim == "white", 1, 0)) %>% 
  mutate(victim_pacific_island = ifelse(race_of_victim == "native hawaiian or other pacific islander",1, 0)) %>% 
  mutate(victim_american_indian = ifelse(race_of_victim == "american indian/alaskan native",1, 0))

nibrs <- nibrs %>% 
  select(-matches("weapon"))


nibrs_aggregate <- nibrs %>% 
  group_by(incident_date, ori) %>% 
  summarize(across(c(rape, sexual_assault_object, fondling, rape_statutory, theft, college_age_rape,
                     college_age_sexual_assault_object, college_age_rape_statutory, 
                     college_age_fondle,
                     victim_black, victim_asian, victim_white, victim_pacific_island, victim_american_indian),
                   ~ sum(., na.rm = T))) %>% 
  ungroup()

write_csv(nibrs_aggregate, file = "Created Data/nibrs/nibrs_aggregated.csv")
