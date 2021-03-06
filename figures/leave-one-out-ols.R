## Purpose of script: Leave-one-out regressions OLS
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-11
##

library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}

if(!exists("daily_crime_weekends")) {
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if(!exists("daily_crime_weekdays")) {
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv")
}

# daily_crime <- daily_crime %>% 
#   filter(university != "University of North Florida") ## not sure whether to keep in or not

distinct_universities <- daily_crime %>% 
  distinct(university) %>% 
  pull()


loo_function <- function(dependent_var, data){
  count <- 1
  for (uni in distinct_universities) {
    if (count == 1) {
      model_original <- ifc::reghdfe(data, 
                            dependent_var, "treatment", c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred"), 'university') %>% 
        broom::tidy(conf.int = T) %>% 
        mutate(original_regression = "All Universities")
      model <- ifc::reghdfe(data %>% filter(university != uni) , 
                     dependent_var, "treatment", c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred"), 'university') %>% 
        broom::tidy(conf.int = T) %>% 
        mutate(original_regression = "Leave-One-Out")
      final_results<- model_original %>% 
        bind_rows(model)
    }
    else {
      model <- ifc::reghdfe(data %>% filter(university != uni),
                            dependent_var, "treatment", c("day_of_week", "university_by_academic_year", "holiday", "spring_semester","game_occurred"),cluster = 'university')
      final_results_append <- broom::tidy(model, conf.int = T)[1,] %>% 
        mutate(original_regression = "Leave-One-Out")
      final_results<- final_results %>% 
        bind_rows(final_results_append)
    }
    count <- count + 1
  }
  return(final_results)
}


# putting together data for looping ---------------------------------------
datas <- list(daily_crime, daily_crime_weekends ,daily_crime_weekdays)
week_type <- tibble(week_type = c(rep("All Days", 38), rep("Weekends", 38), rep("Weekdays", 38)),
                    row_number = c(1:38, 1:38, 1:38))



# loo regressions ---------------------------------------------------------

alc_loo <- map_df(datas, ~loo_function("alcohol_offense_per25", .x)) %>% 
  bind_cols(week_type)

sex_loo <- map_df(datas, ~loo_function("sexual_assault_per25", .x)) %>% 
  bind_cols(week_type)


# loo plot function -------------------------------------------------------

leave_one_out_plot <- function(data) {
  plot <- data %>% 
    ggplot(aes(row_number, estimate, color = factor(original_regression))) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
    facet_wrap(~week_type) +
    theme_minimal() +
    geom_hline(aes(yintercept = 0), color = "dark red") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
          legend.position = "bottom") +
    labs(y = "Coefficient Estimate and 95% Confidence Interval", color = " ")
  return(plot)
}

# loo plot function -------------------------------------------------------


loo_sex_ols <- leave_one_out_plot(sex_loo)  + scale_color_manual(values =c("4DBBD5B2", "black"))
loo_alc_ols <- leave_one_out_plot(alc_loo) + scale_color_manual(values =c("4DBBD5B2", "black"))

