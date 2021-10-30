## Author: Michael Topper
##
## Date Last Edited: 2021-03-31
##

library(modelsummary)
library(tidyverse)
library(kableExtra)

## Note that this is omitting the summer months of 6, 7, and 8. 
if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv") %>% 
    filter(university %in% ifc::moratorium_schools())
}

summary_stats_crime <- daily_crime %>% 
  mutate(days_week = case_when(
    day_of_week == "Mon" | day_of_week == "Tue" | day_of_week == "Wed" | day_of_week == "Thu" ~ "Monday - Thursday",
    day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun" ~ "Friday - Sunday"
  )) %>% 
  mutate(moratorium = ifelse(treatment == 1, "In Effect", "No Moratorium")) %>% 
  datasummary(((`Alcohol Offense` = alcohol_offense_per25) + (`Sexual Assault` = sexual_assault_per25 ) +
                 (`Drug Offense` = drug_offense_per25) +
                 (`Robbery/Burglary` = robbery_burglary_per25)) * (`Moratorium` = moratorium) ~ days_week *(Mean + SD + Min + Max) ,
              data = .,
              title = "Summary statistics for alcohol offenses, sexual assault, and robbery/burglary.",
              notes = "Outcome variables are per 25,000 enrolled students.")


