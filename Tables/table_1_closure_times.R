

## This file creates the main source table for showing my variation of policy enactment over fraternity/sorority life.

library(tidyverse)
library(lubridate)

# ## loading in the ipeds data so I can get the universities I use
# ipeds <- read_csv("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/IPEDS/Data_11-3-2020---571.csv")
# 
# ## update this if you get rid of a school from the sample
# ipeds <- ipeds %>% 
#   filter(`Institution Name`!= "Johns Hopkins University") %>% 
#   filter(`Institution Name`!= "CUNY Bernard M Baruch College", `Institution Name`!= "Emory University", `Institution Name`!= "California State University-Chico")

spreadsheet <- readxl::read_xlsx("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/spreadsheet_final.xlsx")

spreadsheet <- janitor::clean_names(spreadsheet)
  
# universities <- ipeds[1:2]

spreadsheet <-  spreadsheet[1:14]

# spreadsheet <- bind_cols(spreadsheet, universities)

spreadsheet <- janitor::clean_names(spreadsheet)


spreadsheet <- spreadsheet %>% 
  mutate(date = as.Date(date), date2 = as.Date(date2), deadline = as.Date(deadline), deadline2 = as.Date(deadline2)) %>% 
  mutate(length_1 = deadline - date, length_2 = deadline2 - date2)

colnames(spreadsheet)
## creating the length of shutdown 
closure_table <- spreadsheet %>% 
  select(university, date, deadline, length_1, university_enacted_1, date2, deadline2, length_2, university_enacted_2)

## putting these universities into the correct order
closure_table <- closure_table %>% 
  mutate(university = gsub("-Main Campus", "", university)) %>% 
  select("University" = university, "Suspension Date" = date, 
         "Suspension End" = deadline, 
         "Length" = length_1, 
         #"University Enacted" =university_enacted_1,
         "Suspension Date (2)" = date2,
         "Suspension End (2)" = deadline2,
         "Length (2)" = length_2)
         #"Suspension (2)" = moratorium_date_2, "Suspension 2 End" = moratorium_end_2)
         #"University Enacted (2)" = university_enacted_2)

total_closures = 43
average_length <- closure_table %>% 
  summarize(length_average = (sum(`Length`,na.rm = T) + sum(`Length (2)`,na.rm = T))/total_closures )



