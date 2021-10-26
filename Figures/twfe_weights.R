## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-25
##

library(tidyverse)
library(TwoWayFEWeights)
if (!exists("daily_crime")) {
  daily_crime <- read_csv(here::here("Created Data/xMaster_data_2021/daily_panel.csv")) 
}
if (!exists("daily_crime_weekdays")) {
  daily_crime_weekdays <- daily_crime %>% 
    filter(day_of_week == "Mon" | day_of_week == "Thu" | day_of_week == "Wed" | day_of_week == "Tue" )
}

if (!exists("daily_crime_weekends")) {
  daily_crime_weekends <- daily_crime %>%
    filter(day_of_week == 'Fri' | day_of_week == "Sat" | day_of_week == "Sun")
}

TwoWayFEWeights::twowayfeweights(daily_crime,
                                 "alcohol_offense_per25",
                                 "university",
                                 "date",
                                 "treatment",
                                 cmd_type = "feTR",
                                 controls = c("week_before", "week_after"))
TwoWayFEWeights::twowayfeweights(daily_crime,
                                 "alcohol_offense_per25",
                                 "university",
                                 "semester_number",
                                 "treatment",
                                 cmd_type = "feTR",
                                 controls = c("week_before", "week_after"))
