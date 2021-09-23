## Purpose of script: combiens clery crime data to check how different daily crime logs and clery crime is
##
## Author: Michael Topper
##
## Date Last Edited: 2021-09-23
##

library(tidyverse)
library(lubridate)
library(tidytext)

discipline <- read_csv("Created Data/Clery_act_data/discipline.csv")
crimes <- read_csv("Created Data/Clery_act_data/crime.csv")

clery <- discipline %>% 
  left_join(crimes)

## According to the CSH help center email ("question on clery act data") We have:
# 1.The three primary geographical categories (on campus, noncampus, and public property) are mutually exclusive. However, On-campus student housing facilities are located on campus, and thus are a subset of on campus.
# 
# 2.Institutions are required to disclose the number of reported crimes, not the results of an investigation unless the report is officially unfounded by law enforcement.
# 
# 3.Rape and statutory rape are not the same thing. For an incident to be considered statutory rape it must be consensual beyond the age limitations of local laws. If an incident is nonconsensual then it is rape.


clery <- clery %>% 
  mutate(clery_total_rape = noncampus_rape + oncampus_rape + publicproperty_rape) %>% 
  mutate(clery_total_drug = noncampus_drug + publicproperty_drug + oncampus_drug) %>% 
  mutate(clery_total_burglary = noncampus_burgla + publicproperty_burgla + oncampus_burgla) %>% 
  mutate(clery_total_liquor = noncampus_liquor + publicproperty_liquor + oncampus_liquor) %>% 
  mutate(clery_total_sexual_assault = noncampus_rape + noncampus_fondl + noncampus_statr + noncampus_inces +
           oncampus_rape + oncampus_fondl + oncampus_statr + oncampus_inces + publicproperty_rape + publicproperty_fondl +
           publicproperty_statr + publicproperty_inces)

yearly_crime <- read_csv("Created Data/xMaster_data_2021/yearly_panel.csv") %>% 
  filter(year > 2013)



combined_crime <- yearly_crime %>% 
  left_join(clery) %>% 
  select(year, university, sexual_assault, alcohol_offense, drug_offense, rape, robbery_burglary,
         starts_with("clery_total"))

combined_crime %>% 
  select(year, university, sexual_assault, clery_total_sexual_assault) %>% 
  pivot_longer(cols = matches("sexual_assault"), names_to = "clery_or_not", values_to = "sexual_assault") %>% 
  ggplot(aes(year, sexual_assault)) +
  geom_path(aes(color = clery_or_not)) +
  facet_wrap(~university, scales = "free_y") 
  
combined_crime %>% 
  select(year, university, alcohol_offense, clery_total_liquor) %>% 
  rename("Daily Crime Logs" = alcohol_offense,
         "Clery Act Official" = clery_total_liquor) %>% 
  pivot_longer(cols = c(`Daily Crime Logs`, `Clery Act Official`), names_to = "clery_or_not", values_to = "alcohol") %>% 
  ggplot(aes(year, alcohol)) +
  geom_path(aes(color = clery_or_not)) +
  facet_wrap(~university, scales = "free_y") 

ifc::never_treated_no_death()
ifc::moratorium_schools()
clery_compare(combined_crime, alcohol_offense, clery_total_liquor, "alcohol")
