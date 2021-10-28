## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-25
##

library(tidyverse)
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

# twfe_semester <- TwoWayFEWeights::twowayfeweights(daily_crime,
#                                  "alcohol_offense_per25",
#                                  "university",
#                                  "semester_number",
#                                  "treatment",
#                                  cmd_type = "feTR",
#                                  controls = c("week_before", "week_after"))

twfe_decomp <- tibble(model_fixed_effects = c("University and Day-by-Month-by-Year", "University and Semester-Number",
                               "University-by-Semester Number and Day-of-Week"),
       positive_weights_sum = c("1", "1.0934", "Cannot be computed."),
       negative_weights_sum = c("0", "-0.0934", "Cannot be computed."),
       number_of_att_negative = c("0", "27", "Cannot be computed."),
       number_of_att_positive = c("2843", "37", "Cannot be computed."))

twfe_weights <- kableExtra::kbl(twfe_decomp,
                booktabs = T,
                col.names = c("Model", "Sum of Positive Weights", "Sum of Negative Weights", "Number of Negative ATT", "Number of Positive ATT"),
                title = "\\label{twfe_weights}DeChaismartin decomposition of TWFE with primary models.")



