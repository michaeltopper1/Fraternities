## Purpose of sUncript: cleans and web scrapes data for virginia
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-09
##

library(tidyverse)
library(rvest)

months <- str_to_lower(month.name)
years <- c(2013:2019)

for (year in years){
  print(year)
  for (month in months){
    print(month)
    if (month == "november" & year == 2013){
      page <- read_html(paste('https://uvapolice.virginia.edu/crime-log/',month,'-30',year, sep = ""))
    }
    else{
      page <- read_html(paste('https://uvapolice.virginia.edu/crime-log/',month,'-',year, sep = ""))
    }
    html_text(page) %>% str_split('\n') %>% unlist() %>% str_to_lower() -> virginia_text
    virginia_text %>% str_detect("^rpt:") %>%  which -> report_indices
    virginia_text %>% str_detect("^rpt:") %>% which %>% magrittr::add(-2) -> incident_indices
    virginia_report_dates <- virginia_text[report_indices] %>% 
      as_tibble() %>% 
      separate(value, into = c("date_reported", "date_occurred"), sep = "occ:") %>% 
      extract(date_reported, c("time_reported", "date_reported"),
              '(\\d{4})\\s(\\d{1,2}-\\d{1,2}-\\d{1,2})') %>% 
      extract(date_occurred, c("time_occurred", "date_occurred"),
              '(\\d{4})\\s(\\d{1,2}-\\d{1,2}-\\d{1,2})')
    virginia_incidents <- virginia_text[incident_indices] %>% 
      as_tibble() %>% 
      separate(value, c("incident", "other"), sep = "\\s{0,1}–\\s{0,1}|\\d{5,}|\\s{0,1}-\\s", extra = "merge") %>% 
      select(incident) 
    if (month == "january" & year == 2013){
      virginia <- bind_cols(virginia_report_dates, virginia_incidents)
    }
    else {
      virginia_append <- bind_cols(virginia_report_dates, virginia_incidents)
      virginia <- virginia %>% 
        bind_rows(virginia_append)
    }
  }
}

virginia <- virginia %>% 
  mutate(case_number = paste("00",row_number(), sep = "")) %>% 
  filter(incident != "") %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(location = NA, university = "University of Virginia-Main Campus")

virginia <- virginia %>% 
  mutate(across(starts_with("time"), ~format(strptime(., format = "%H%M"), format = "%H:%M")))
write_csv(virginia, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/virginia.csv")
