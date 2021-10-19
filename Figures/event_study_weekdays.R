## Purpose of script: creates the 'multiple event' event study using weekly data
##
## Author: Michael Topper
##
## Date Last Edited: 2021-07-19
##

library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)


# choose the event window -------------------------------------------------

lead_endpoint <- 8
lag_endpoint <- 8
leads <- c(1:lead_endpoint)
lags <- c(1:lag_endpoint)


# loading in the data -----------------------------------------------------
weekly_crime_weekdays <- read_csv("Created Data/xMaster_data_2021/weekly_panel_weekdays.csv") %>% 
  filter(university %in% ifc::moratorium_schools())

# for no sexual assault schools
weekly_crime_weekends <- weekly_crime_weekends %>%
  mutate(sexual_assault_school = ifelse(reason1 %in% c("sexual assault") | reason2 %in% c("sexual assault"), 1,0)) %>%
  filter(sexual_assault_school == 0)


# matching closure dates to week numbers and create treatment  --------
## note that beta_0 is my treatment variable
## the process is as follows: floor date the closure dates and match to the week that it is floor dated to

weekly_crime_weekdays <- weekly_crime_weekdays %>% 
  mutate(closure_1_floor = lubridate::floor_date(closure_1, unit = "week", week_start = 1)) %>%
  mutate(closure_2_floor = floor_date(closure_2, unit = "week",week_start = 1)) %>% 
  relocate(week, closure_1, closure_1_floor) %>% 
  mutate(beta_0 = ifelse(closure_1_floor == week | closure_2_floor == week, 1, 0)) %>% 
  mutate(beta_0 = ifelse(is.na(beta_0), 0, beta_0)) %>% 
  relocate(beta_0)


# creating leads and lags -------------------------------------------------

for (i in leads) {
  column_name <- paste0("beta_minus_",i)
  weekly_crime_weekdays <- weekly_crime_weekdays %>% 
    mutate(!!sym(column_name) := ifelse(dplyr::lead(beta_0, n = i) == 1,1,0 ))
}

for (i in leads) {
  column_name <- paste0("beta_plus_",i)
  weekly_crime_weekdays <- weekly_crime_weekdays %>% 
    mutate(!!sym(column_name) := ifelse(dplyr::lag(beta_0, n = i) == 1,1,0 ))
}

weekly_crime_weekdays <- weekly_crime_weekdays %>% 
  mutate(across(starts_with("beta"), ~ifelse(is.na(.), 0, .)))

## now need to create the binned column which will be the sum of the 1s - create function for this. need to arrange this 

## getting the first lead to be binned
last_lead <- paste0("beta_minus_", lead_endpoint)
weekly_crime_weekdays <- weekly_crime_weekdays %>% 
  group_by(university) %>% 
  arrange(desc(week)) %>% 
  mutate(beta_lead_binned = ifelse(!!sym(last_lead) ==1, 1, NA)) %>% 
  mutate(beta_lead_binned = ifelse(!!sym(last_lead) > 0, cumsum(!!sym(last_lead)), NA)) %>% 
  relocate(beta_lead_binned, starts_with("beta")) %>% 
  fill(beta_lead_binned, .direction = "down")  %>% ungroup() %>% arrange(week)


## getting the last lag to be binned
last_lag <- paste0("beta_plus_", lag_endpoint)
weekly_crime_weekdays <- weekly_crime_weekdays %>% 
  group_by(university) %>% 
  arrange(university,week) %>% 
  mutate(beta_lag_binned = ifelse(!!sym(last_lag) == 1, 1, NA)) %>% 
  mutate(beta_lag_binned = ifelse(!!sym(last_lag) > 0, cumsum(!!sym(last_lag)), NA)) %>% 
  relocate(beta_lag_binned) %>% 
  fill(beta_lag_binned, .direction = "down")  %>% ungroup()

## changing any NA values to zeros
weekly_crime_weekdays <- weekly_crime_weekdays %>% 
  mutate(across(c(beta_lead_binned, beta_lag_binned), ~ifelse(is.na(.), 0, .)))


# getting semester numbers ------------------------------------------------

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv")

daily_crime <- daily_crime %>% 
  mutate(date_floor = floor_date(date, unit = "week", week_start = 1))

semester_numbers <- daily_crime %>% 
  select(semester_number, date_floor, university)

## this gets rid of the observation duplication problem
semester_numbers <- semester_numbers %>% 
  group_by(university) %>% 
  distinct(date_floor, semester_number) %>% 
  ungroup()

## joining together to get semester number in the data
weekly_crime_weekdays <- weekly_crime_weekdays %>% 
  left_join(semester_numbers, by = c("week" = "date_floor", "university" = "university")) 



# estimating the event studies --------------------------------------------


es_alc <- weekly_crime_weekdays %>% 
  group_by(university, semester_number) %>% 
  mutate(university_by_semester_number = cur_group_id()) %>% 
  ungroup() %>% 
  feols(alcohol_offense_per25 ~ beta_lead_binned + beta_minus_7 + beta_minus_6 + beta_minus_5 + beta_minus_4 + beta_minus_3 + beta_minus_2 + beta_0 +
          beta_plus_1 + beta_plus_2 + beta_plus_3 + beta_plus_4 + beta_plus_5 +
          beta_plus_6 + beta_plus_7 + beta_lag_binned| university_by_semester_number , cluster = ~university, data = .) 
es_sex <- weekly_crime_weekdays %>% 
  group_by(university, semester_number) %>% 
  mutate(university_by_semester_number = cur_group_id()) %>% 
  ungroup() %>% 
  feols(sexual_assault_per25 ~ beta_lead_binned + beta_minus_7 + beta_minus_6 + beta_minus_5 + beta_minus_4 + beta_minus_3 + beta_minus_2 + beta_0 +
          beta_plus_1 + beta_plus_2 + beta_plus_3 + beta_plus_4 + beta_plus_5 +
          beta_plus_6 + beta_plus_7 + beta_lag_binned| university_by_semester_number, cluster = ~university, data = .) 


# creating graphing function ----------------------------------------------


event_study_func <- function(x, window_size) {
  coefs <- broom::tidy(x, conf.int =T)[1:(2*window_size), ]
  coefs <- coefs %>% 
    add_row(.before = window_size)
  event_window <- c(-window_size:window_size)
  event_window <- as_tibble(event_window)  
  coefs <- tibble(coefs, event_window)
  coefs <- coefs %>% 
    mutate(across(.cols = -c(term), ~ ifelse(is.na(.), 0, .))) 
  coefs <- coefs[2:(2*window_size),]
  plot <- coefs %>% 
    ggplot(aes(x = value, y = estimate), alpha = 0.8) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),fill = "light grey", alpha =0.8) +
    geom_path(linetype = "dashed", color = "black") +
    geom_point() +
    # geom_errorbar(aes(x = value, ymin = conf.low, ymax = conf.high), alpha = 0.8) +
    # geom_rect(aes(xmin = -0.5, xmax =0.5, ymin = -Inf, ymax = Inf), fill = "light blue", alpha = 0.03) +
    scale_x_continuous(labels = c(-window_size:window_size), breaks = c(-window_size:window_size)) +
    labs(x = "Weeks to Moratorium", y = "Coefficient Estimate") +
    theme_minimal()
  return(plot)
}


# plotting the graphs -----------------------------------------------------


es_sex <- event_study_func(es_sex, 8) +
  geom_hline(yintercept = 0, color = "dark red") 
es_alcohol <- event_study_func(es_alc, 8) +
  geom_hline(yintercept = 0, color = "dark red")

