
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
weekly_crime <- read_csv("created_data/xmaster_data/weekly_panel.csv") %>% 
  filter(university %in% ifc::moratorium_schools())



# matching closure dates to week numbers and create treatment  --------
## note that beta_0 is my treatment variable
## the process is as follows: floor date the closure dates and match to the week that it is floor dated to

weekly_crime <- weekly_crime %>% 
  mutate(closure_1_floor = lubridate::floor_date(closure_1, unit = "week", week_start = 1)) %>%
  mutate(closure_2_floor = floor_date(closure_2, unit = "week",week_start = 1)) %>% 
  relocate(week, closure_1, closure_1_floor) %>% 
  mutate(beta_0 = ifelse(closure_1_floor == week | closure_2_floor == week, 1, 0)) %>% 
  mutate(beta_0 = ifelse(is.na(beta_0), 0, beta_0)) %>% 
  relocate(beta_0)


# creating leads and lags -------------------------------------------------

for (i in leads) {
  column_name <- paste0("beta_minus_",i)
  weekly_crime <- weekly_crime %>% 
    mutate(!!sym(column_name) := ifelse(dplyr::lead(beta_0, n = i) == 1,1,0 ))
}

for (i in leads) {
  column_name <- paste0("beta_plus_",i)
  weekly_crime <- weekly_crime %>% 
    mutate(!!sym(column_name) := ifelse(dplyr::lag(beta_0, n = i) == 1,1,0 ))
}

weekly_crime <- weekly_crime %>% 
  mutate(across(starts_with("beta"), ~ifelse(is.na(.), 0, .)))

## now need to create the binned column which will be the sum of the 1s - create function for this. need to arrange this 

## getting the first lead to be binned
last_lead <- paste0("beta_minus_", lead_endpoint)
weekly_crime <- weekly_crime %>% 
  group_by(university) %>% 
  arrange(desc(week)) %>% 
  mutate(beta_lead_binned = ifelse(!!sym(last_lead) ==1, 1, NA)) %>% 
  mutate(beta_lead_binned = ifelse(!!sym(last_lead) > 0, cumsum(!!sym(last_lead)), NA)) %>% 
  relocate(beta_lead_binned, starts_with("beta")) %>% 
  fill(beta_lead_binned, .direction = "down")  %>% ungroup() %>% arrange(week)


## getting the last lag to be binned
last_lag <- paste0("beta_plus_", lag_endpoint)
weekly_crime <- weekly_crime %>% 
  group_by(university) %>% 
  arrange(university,week) %>% 
  mutate(beta_lag_binned = ifelse(!!sym(last_lag) == 1, 1, NA)) %>% 
  mutate(beta_lag_binned = ifelse(!!sym(last_lag) > 0, cumsum(!!sym(last_lag)), NA)) %>% 
  relocate(beta_lag_binned) %>% 
  fill(beta_lag_binned, .direction = "down")  %>% ungroup()

## changing any NA values to zeros
weekly_crime <- weekly_crime %>% 
  mutate(across(c(beta_lead_binned, beta_lag_binned), ~ifelse(is.na(.), 0, .)))


# getting semester numbers ------------------------------------------------

daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")

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
weekly_crime <- weekly_crime %>% 
  left_join(semester_numbers, by = c("week" = "date_floor", "university" = "university")) 

weekly_crime_omit <- weekly_crime %>% 
  filter(beta_minus_1 != 1)



# creating graphing function for omitting period before----------------------------------------------
event_study_func_omit <- function(x){
  weeks <- tibble(weeks = c(-7:-2, 0:7))
  graph <- x %>% 
    broom::tidy(conf.int =T) %>% 
    add_row(.before = 7) %>% 
    mutate(across(.cols = -c(term), ~ ifelse(is.na(.), 0, .))) %>% 
    slice(2:15) %>% 
    bind_cols(weeks) %>% 
    ggplot(aes(x = as.factor(weeks), y = estimate, group = 1), alpha = 0.8) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high),fill = "light grey", alpha =0.8) +
    geom_path(linetype = "dashed", color = "black") +
    geom_point() +
    # geom_errorbar(aes(x = value, ymin = conf.low, ymax = conf.high), alpha = 0.8) +
    # geom_rect(aes(xmin = -0.5, xmax =0.5, ymin = -Inf, ymax = Inf), fill = "light blue", alpha = 0.03) +
    scale_x_discrete(labels = c(-7:-2, 0:7), breaks = c(-7:-2, 0:7)) +
    geom_hline(yintercept = 0, color = "dark red") +
    labs(x = "Weeks to Moratorium", y = "Coefficient Estimate") +
    theme_minimal()
  return(graph)
}



es_sex_omit <- weekly_crime_omit %>% 
  group_by(university, semester_number) %>% 
  mutate(university_by_semester_number = cur_group_id()) %>% 
  ungroup() %>% 
  feols(sexual_assault_per25 ~ beta_lead_binned + beta_minus_7 + beta_minus_6 + beta_minus_5 + beta_minus_4 + beta_minus_3  + beta_0 +
          beta_plus_1 + beta_plus_2 + beta_plus_3 + beta_plus_4 + beta_plus_5 +
          beta_plus_6 + beta_plus_7 + beta_lag_binned| university_by_semester_number, cluster = ~university, data = .) %>% 
  event_study_func_omit()


