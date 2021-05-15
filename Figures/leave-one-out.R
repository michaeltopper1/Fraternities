## Purpose of script: Leave-one-out regressions
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-11
##

library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv",
                        guess_max = 50000)

daily_crime <- daily_crime %>% 
  filter(month !=7 & month !=6) %>%
  filter(university != "University of North Florida") ## not sure whether to keep in or not

distinct_universities <- daily_crime %>% 
  distinct(university) %>% 
  pull()

count <- 1
for (uni in distinct_universities) {
  if (count == 1) {
    model <- daily_crime %>% 
      filter(university != uni) %>% 
      fepois(sexual_assault ~ treatment + total_price_oncampus_is + total_price_oncampus_oos + total_price_offcampus_is +
               frac_admitted_total + fulltime_retention_rate + undergraduate_enrollment + 
               frac_total_black + frac_total_hispanic_latino + frac_total_asian + total_enrollment + graduation_rate_total_cohort|
              uni_month + weekday + year,
             cluster = ~university, data = .)
    final_results_sex <- broom::tidy(model, conf.int = T)[1,]
  }
  else {
    model <- daily_crime %>% 
      filter(university != uni) %>% 
      fepois(sexual_assault ~ treatment + total_price_oncampus_is + total_price_oncampus_oos + total_price_offcampus_is +
               frac_admitted_total + fulltime_retention_rate + undergraduate_enrollment + 
               frac_total_black + frac_total_hispanic_latino + frac_total_asian + total_enrollment + graduation_rate_total_cohort|
               uni_month + weekday + year,
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
      fepois(alcohol_offense ~ treatment + total_price_oncampus_is + total_price_oncampus_oos + total_price_offcampus_is +
               frac_admitted_total + fulltime_retention_rate + undergraduate_enrollment + 
               frac_total_black + frac_total_hispanic_latino + frac_total_asian + total_enrollment + graduation_rate_total_cohort|
               uni_month + weekday + year,
             cluster = ~university, data = .)
    final_results_alc <- broom::tidy(model, conf.int = T)[1,]
  }
  else {
    model <- daily_crime %>% 
      filter(university != uni) %>% 
      fepois(alcohol_offense ~ treatment + total_price_oncampus_is + total_price_oncampus_oos + total_price_offcampus_is +
               frac_admitted_total + fulltime_retention_rate + undergraduate_enrollment + 
               frac_total_black + frac_total_hispanic_latino + frac_total_asian + total_enrollment + graduation_rate_total_cohort|
               uni_month + weekday + year,
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
    ggthemes::theme_fivethirtyeight() +
    geom_hline(aes(yintercept = 0), color = "red") +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank())
  return(plot)
}

loo_sex <- leave_one_out_plot(final_results_sex) 
loo_alc <- leave_one_out_plot(final_results_alc)
