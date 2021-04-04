###############################################################################
##  This file cleans out all years of florida state into the desired format  ##
###############################################################################



library(tidyverse)
library(readxl)
library(lubridate)


years <- c(2013:2019)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida State University/crime_daily_"

## this loops through all years and then appends and cleans them out to the desired format
for (year in years) {
  if (year == 2013) {
    florida_state_cleaned <- read_xlsx(
      paste(path, year, ".xlsx", sep = "")
    )
    florida_state_cleaned <- florida_state_cleaned %>% 
      janitor::clean_names() %>% 
      select(-disposition) %>% 
      rename("incident" = nature, "date_reported" = when_reported,
             "date_occurred" = occurred_between) %>% 
      separate(date_reported, into = c("date_reported", "time_reported"), sep = "\\s") %>% 
      extract(date_occurred, into = "time_occurred", regex = "(\\s\\d\\d:\\d\\d:\\d\\d\\s)", remove = F) %>%
      mutate(time_occurred = str_trim(time_occurred)) %>% 
      extract(date_occurred, into = "date_occurred", regex = "(\\d\\d\\d\\d-\\d\\d-\\d\\d)") %>% 
      mutate(time_occurred = format(strptime(time_occurred,"%H:%M"), format = "%H:%M")) %>% 
      mutate(time_reported = format(strptime(time_reported,"%H:%M"), format = "%H:%M")) %>% 
      mutate(university = "Florida State University")
  }
  else {
    florida_state_append <- read_xlsx(
      paste(path, year, ".xlsx", sep = "")
    )
    florida_state_append <- florida_state_append %>% 
      janitor::clean_names() %>% 
      select(-disposition) %>% 
      rename("incident" = nature, "date_reported" = when_reported,
             "date_occurred" = occurred_between) %>% 
      separate(date_reported, into = c("date_reported", "time_reported"), sep = "\\s") %>% 
      extract(date_occurred, into = "time_occurred", regex = "(\\s\\d\\d:\\d\\d:\\d\\d\\s)", remove = F) %>%
      mutate(time_occurred = str_trim(time_occurred)) %>% 
      extract(date_occurred, into = "date_occurred", regex = "(\\d\\d\\d\\d-\\d\\d-\\d\\d)") %>% 
      mutate(time_occurred = format(strptime(time_occurred,"%H:%M"), format = "%H:%M")) %>% 
      mutate(time_reported = format(strptime(time_reported,"%H:%M"), format = "%H:%M")) %>% 
      mutate(university = "Florida State University")
    florida_state_cleaned <- florida_state_cleaned %>% 
      bind_rows(florida_state_append)
  }
}


write_csv(florida_state_cleaned, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/florida_state.csv")
