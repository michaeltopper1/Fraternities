## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-25
##

library(tidyverse)
library(kableExtra)

# semester_level <- read_csv("created_data/xmaster_data/semester_level.csv")
# 
# 
# daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
# library(TwoWayFEWeights)
# if (!exists("daily_crime")) {
#   daily_crime <- read_csv(here::here("Created Data/xMaster_data_2021/daily_panel.csv")) 
# }
# if (!exists("daily_crime_weekdays")) {
#   daily_crime_weekdays <- daily_crime %>% 
#     filter(day_of_week == "Mon" | day_of_week == "Thu" | day_of_week == "Wed" | day_of_week == "Tue" )
# }
# 
# if (!exists("daily_crime_weekends")) {
#   daily_crime_weekends <- daily_crime %>%
#     filter(day_of_week == 'Fri' | day_of_week == "Sat" | day_of_week == "Sun")
# }

# twfe <- TwoWayFEWeights::twowayfeweights(daily_crime,
#                                  "alcohol_offense_per25",
#                                  "university",
#                                  "date",
#                                  "treatment",
#                                  cmd_type = "feTR",
#                                  controls = c("week_before", "week_after"))


# TwoWayFEWeights::twowayfeweights(semester_level,
#                                  "alcohol_offense_per25",
#                                  "university_by_semester_number",
#                                  "semester_number",
#                                  "treatment",
#                                  cmd_type = "feTR",
#                                  controls = c("lead_1", "lead_2", "lag_1", "lag_2"))

twfe_decomp <- tibble(model_fixed_effects = c("University and Day-by-Month-by-Year", "University and Semester-by-Year",
                               "University-by-Semester Number and Semester-by-Year",
                               "University and Semester-by-Year", "University-by-Semester-Number and Semester-by-Year"),
       positive_weights_sum = c("1.046", "14.55", "1.056", "1.01", "1.019"),
       negative_weights_sum = c("-0.04", "-13.55","-0.056", "-0.01", "-0.019"),
       number_of_att_negative = c("760", "28", "982", "12", "14"),
       number_of_att_positive = c("2843", "36", "1861","52", "50"))


## these numbers asssume that the lead and lags are correct
twfe_weights <- kbl(twfe_decomp,
                booktabs = T,
                col.names = c("Model", "Sum of Positive Weights", "Sum of Negative Weights", "Number of Negative ATT", "Number of Positive ATT"),
                caption = "\\label{twfe_weights}DeChaismartin decomposition of TWFE with primary models.") %>% 
  kable_styling() %>% 
  pack_rows("Daily-level Data", 1, 3) %>% 
  pack_rows("Semester-level", 4, 5)



