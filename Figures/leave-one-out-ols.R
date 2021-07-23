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
  daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv")
}


# daily_crime <- daily_crime %>% 
#   filter(university != "University of North Florida") ## not sure whether to keep in or not

distinct_universities <- daily_crime %>% 
  distinct(university) %>% 
  pull()

count <- 1

for (uni in distinct_universities) {
  if (count == 1) {
    model <- daily_crime %>% 
      filter(university != uni) %>% 
      feols(sexual_assault_per25 ~ treatment |
               uni_semester + weekday,
             cluster = ~university, data = .)
    final_results_sex <- broom::tidy(model, conf.int = T)[1,]
  }
  else {
    model <- daily_crime %>% 
      filter(university != uni) %>% 
      feols(sexual_assault_per25 ~treatment |
               uni_semester + weekday,
             cluster = ~university, data = .)
    final_results_append <- broom::tidy(model, conf.int = T)[1,]
    final_results_sex <- final_results_sex %>% 
      bind_rows(final_results_append)
  }
  count <- count + 1
}


count <- 1
for (uni in distinct_universities) {
  if (count == 1) {
    model <- daily_crime %>% 
      filter(university != uni) %>% 
      feols(alcohol_offense_per25 ~ treatment |
               uni_semester + weekday,
             cluster = ~university, data = .)
    final_results_alc <- broom::tidy(model, conf.int = T)[1,]
  }
  else {
    model <- daily_crime %>% 
      filter(university != uni) %>% 
      feols(alcohol_offense_per25 ~ treatment|
               uni_semester + weekday,
             cluster = ~university, data = .)
    final_results_append <- broom::tidy(model, conf.int = T)[1,]
    final_results_alc <- final_results_alc %>% 
      bind_rows(final_results_append)
  }
  count <- count + 1
}

## leave-one-out plot
leave_one_out_plot <- function(data) {
  plot <- data %>% 
    mutate(row_number = row_number()) %>% 
    ggplot(aes(row_number, estimate)) +
    geom_point() +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
    theme_minimal() +
    geom_hline(aes(yintercept = 0), color = "red") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank()) +
    labs(y = "Coefficient Estimate")
  return(plot)
}

loo_sex_ols <- leave_one_out_plot(final_results_sex) 
loo_alc_ols <- leave_one_out_plot(final_results_alc) 
