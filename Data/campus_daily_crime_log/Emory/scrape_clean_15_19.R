
library(tidyverse)
library(rvest)
library(lubridate)

## This function gets the columns of interest from webscraping
get_columns <- function(link) {
  emory <- read_html(link) %>% 
    html_nodes(".is") %>% 
    html_text() %>% 
    unlist()
  print("next one")
  emory %>% str_detect("^\\d{6,}") %>% which -> case_number
  emory %>% str_detect("^\\d{6,}") %>% which %>% magrittr::add(1) -> date_reported
  emory %>% str_detect("^\\d{6,}") %>% which %>% magrittr::add(2) -> incident
  emory %>% str_detect("^\\d{6,}") %>% which %>% magrittr::add(3) -> date_occurred
  emory %>% str_detect("^\\d{6,}") %>% which %>% magrittr::add(5) -> location
  output <- tibble("case_number" = emory[case_number], 
                   "date_reported" = emory[date_reported],
                   "incident" = emory[incident],
                   "date_occurred" = emory[date_occurred],
                   "location" = emory[location])
  return(output)
}

## the counter to loop through the 
page_number <- as.list(seq(0, 3490, 10))


## webscrapes the data and puts into nice form
emory <- map(page_number, ~get_columns(
  paste("https://emap.fmd.emory.edu/website/CrimeLogPublic/CrimeLog.aspx?st=",., "&sort=&order=asc&fd=&td=&kw=&sta=&fire=N&camp=Atlanta", sep = ""))) %>% 
  reduce(bind_rows)

## cleans the data
emory_15_19 <- emory %>% 
  extract(date_occurred, c("date_occurred", "time_occurred"), 
          "(\\d{1,2}/\\d{1,2}/\\d{4})\\s(\\d{1,2}:\\d{1,2})") %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(university = "Emory University", time_reported = NA) %>% 
  mutate(year = year(date_reported)) %>% 
  filter(year < 2020) %>% 
  select(-year)

write_csv(emory_15_19, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/emory.csv")
