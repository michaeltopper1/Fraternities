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

# daily_crime <- daily_crime %>% 
#   mutate(weekday = case_when(
#     day_of_week == "Mon" ~ 1,
#     day_of_week == "Tue" ~2,
#     day_of_week == "Wed" ~3,
#     day_of_week == "Thu" ~4,
#     day_of_week == "Fri" ~5,
#     day_of_week == "Sat" ~6,
#     day_of_week == "Sun" ~7
#   ))
# TwoWayFEWeights::twowayfeweights(daily_crime,
#                                  "alcohol_offense_per25",
#                                  "university_by_semester_number",
#                                  "semester_number",
#                                  "treatment",
#                                  cmd_type = "feTR")

twfe_decomp <- tibble(model_fixed_effects = c("University and Day-by-Month-by-Year", "University and Semester-by-Year",
                               "University-by-Semester Number and Semester-by-Year"),
       positive_weights_sum = c("1", "1.01", "NA"),
       negative_weights_sum = c("0", "-0.01","NA"),
       number_of_att_negative = c("0", "10", "NA"),
       number_of_att_positive = c("2865", "64", "NA"))


## these numbers asssume that the lead and lags are correct
twfe_weights <- kbl(twfe_decomp,
                booktabs = T,
                col.names = c("Model", "Sum(+ Weights)", "Sum(- Weights)", "Count(Negative ATT)", "Count(Positive ATT)"),
                caption = "\\label{twfe_weights}DeChaismartin decomposition of TWFE with primary models.
                Reading from left to right: the first column represents the model specification,
                the second column represents the sum of the positive weights, the third column represents the sum of the negative weights,
                the fourth column represents the number of negative average treatment effects on the treated,
                and the fifth column represents the number of postiive average treatment effects on the treated.
                Note that NAs mean that the TWFEWeights package cannot estimate negative weights for a specification. ") %>% 
  kable_styling() %>% 
  column_spec(1, width = "20em") 



