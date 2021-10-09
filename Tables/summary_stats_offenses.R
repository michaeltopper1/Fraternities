## Author: Michael Topper
##
## Date Last Edited: 2021-03-31
##

library(modelsummary)
library(tidyverse)
library(kableExtra)

## Note that this is omitting the summer months of 6, 7, and 8. 
if(!exists("daily_crime")) {
  daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv") %>% 
    filter(university %in% ifc::moratorium_schools())
}

summary_stats_crime <- daily_crime %>% 
  mutate(days_week = case_when(
    weekday == "Mon" | weekday == "Tue" | weekday == "Wed" | weekday == "Thu" ~ "Monday - Thursday",
    weekday == "Fri" | weekday == "Sat" | weekday == "Sun" ~ "Friday - Sunday"
  )) %>% 
  mutate(moratorium = ifelse(treatment == 1, "In Effect", "No Moratorium")) %>% 
  datasummary(((`Alcohol Offense` = alcohol_offense_per25) + (`Sexual Assault` = sexual_assault_per25 ) +
                 (`Robbery/Burglary` = robbery_burglary_per25)) * (`Moratorium` = moratorium) ~ days_week *(Mean + SD + Min + Max) ,
              data = .,
              title = "Summary statistics for alcohol offenses, sexual assault, and robbery/burglary.",
              notes = "Outcome variables are per 25,000 enrolled students.")


