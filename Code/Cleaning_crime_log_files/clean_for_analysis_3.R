## Purpose of script: Get the Data to have the correct specifications for analysis
## Going to exclude summer months for schools on semester/quarter
## Transform some of the outcomes to logs and IHS
## Author: Michael Topper
##
## Date Last Edited: 2021-03-03
##

library(tidyverse)
library(lubridate)
library(haven)

daily_crime <- read_csv("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/xMaster_data_2021/daily_panel.csv",
                        guess_max = 50000)


#### Part 1: Excluding summer months 
## pulling the universities in my sample
universities <- daily_crime %>% 
  distinct(university) %>% pull()
## quarter system schools list
quarter_system_schools <- c("California Polytechnic State University-San Luis Obispo",
                            "Northwestern University")
## the complement of quarter system schools
semester_system_schools <- dplyr::setdiff(universities, quarter_system_schools)

## filtering out the summer months for quarter and semester schools
daily_crime <- daily_crime %>% 
  filter(!(university %in% quarter_system_schools & (month == 7 | month == 8))) %>% 
  filter(!(university %in% semester_system_schools & (month == 6 | month == 7))) 

##### Part 2: Average/Median Closure lengths
## Now I'm getting the average closure length within each university
## I will first do this by getting closure lengths for closure 1 and 2, append, and then group and summarize
closure_1_lengths <- daily_crime %>% 
  group_by(university) %>% 
  mutate(length_closure1 = difftime(closure_1_end, closure_1, units = "days")) %>% relocate(length_closure1) %>% 
  summarize(avg_closure = mean(length_closure1, na.rm = T)) 
closure_2_lengths <- daily_crime %>% 
  group_by(university) %>% 
  mutate(length_closure2 = difftime(closure_2_end, closure_2, units = "days")) %>% relocate(length_closure2) %>% 
  summarize(avg_closure = mean(length_closure2, na.rm = T)) 

## the closure lengths appended
closure_lengths <- bind_rows(closure_1_lengths, closure_2_lengths)

## summarizing the average length within universities to merge with the final data
average_closure_by_university <- closure_lengths %>% 
  group_by(university) %>% 
  summarize(avg_closure_length = mean(avg_closure, na.rm = T))

## merging with the final data
daily_crime <- daily_crime %>% 
  left_join(average_closure_by_university, by = c("university" = "university"))

## Now I will claculate the median of the average closure lengths
median_closure_length <- closure_lengths %>% 
  group_by(university) %>% 
  summarize(avg_closure_length = mean(avg_closure, na.rm = T)) %>% 
  summarize(median = median(avg_closure_length, na.rm = T)) %>% 
  pull()

## creating an indicator for if above the median of the average closure length
daily_crime <- daily_crime %>% 
  mutate(above_median_closure = ifelse(avg_closure_length >= median_closure_length, 1, 0)) 


##### Part 3: Transforming outcome variables of interest to log(x + 1) and IHS log(x + sqrt(x^2 + 1))
## creating function for the ihs
ihs <- function(x) {
  ihs_x <- log(x + sqrt(x^2 + 1))
  return(ihs_x)
}

## creating the IHS variables
daily_crime <- daily_crime %>% 
  mutate(sexual_assault_ihs = ihs(sexual_assault),
         alcohol_offense_ihs = ihs(alcohol_offense),
         robbery_burglary_ihs = ihs(robbery_burglary),
         noise_offense_ihs = ihs(noise_offense),
         theft_ihs = ihs(theft),
         alcohol_offense_strict = ihs(alcohol_offense_strict),
         drug_offense_ihs = ihs(drug_offense))

## creating the total_undergrad columns
daily_crime <- daily_crime %>% 
  mutate(total_undergrad_all = total_undergrad_men + total_undergrad_women,
         total_undergrad_white = total_undergrad_white_men + total_undergrad_white_women)


##### Part 4: Creating the fixed effects 
## first, creating university-by-month fixed effects
daily_crime <- daily_crime %>% 
  group_by(university, month) %>% 
  mutate(university_by_month = cur_group_id()) %>% relocate(university_by_month) %>% 
  ungroup()

## creating university by year by month
daily_crime <- daily_crime %>% 
  group_by(university, month, year) %>% 
  mutate(university_by_month_by_year = cur_group_id()) %>% 
  ungroup()

## creating university by year by week
daily_crime <- daily_crime %>%
  mutate(week = week(date)) %>% 
  group_by(university, year, week) %>% 
  mutate(university_by_year_by_week  = cur_group_id()) %>% 
  ungroup()

## creating university by month
daily_crime <- daily_crime %>% 
  group_by(month, university) %>% 
  mutate(university_by_month =cur_group_id()) %>% 
  ungroup()


daily_crime <- daily_crime %>% 
  group_by(year, month, weekday) %>% 
  mutate(year_by_month_by_weekday = cur_group_id()) %>% 
  ungroup()

daily_crime <- daily_crime %>% 
  group_by(month, year) %>% 
  mutate(year_by_month = cur_group_id()) %>% 
  ungroup()
daily_crime <- daily_crime %>% 
  group_by(university, month) %>% 
  mutate(university_by_month = cur_group_id()) %>% 
  ungroup()

## filtering universities that I do not have closure stuff for
daily_crime <- daily_crime %>% 
  filter(university != "Miami University-Oxford" & university != "Emory University")




write_dta(daily_crime, path = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/xMaster_data_2021/daily_crime_analysis.dta")
write_csv(daily_crime, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/xMaster_data_2021/daily_crime_analysis.csv")