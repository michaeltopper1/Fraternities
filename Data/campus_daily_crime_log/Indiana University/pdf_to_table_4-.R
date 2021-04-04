
library(pdftools)
library(tidyverse)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Indiana University/12-2018/12-23-18.rtf"

indiana <- textreadr::read_rtf(path)
indiana <- str_to_lower(indiana)
indiana %>% str_split('\n') %>% unlist() -> indiana
indiana <- str_trim(indiana)
indiana %>% str_detect("^incident") %>% which -> incidents_indices
indiana %>% str_detect("location") %>% which %>% magrittr::add(1)-> dates_and_time_indices
indiana %>% str_detect("^date reported") %>% which -> date_reported_indices
indiana
indiana[incidents_indices] %>% 
  as_tibble() %>% 
  separate(value, into = c("value","incident"), sep = ":\\s") %>% 
  mutate(across(everything(), ~str_trim(.))) -> incidents

indiana[dates_and_time_indices] %>% 
  as_tibble() %>% 
  separate(value, into = c("is_date", "date"), sep = 'from:|occurred\\sto:') %>% 
  extract(date, into = "date_occurred", regex = "(\\d\\d/\\d\\d/\\d\\d)", remove = F) %>%
  extract(date, into = "time", regex = "(\\d\\d:\\d\\d)") %>% 
  select(date_occurred, time)-> dates_and_time

indiana[date_reported_indices] %>% 
  as_tibble() %>% 
  extract(value, into = "reported", regex = "(\\d\\d/\\d\\d/\\d\\d)")  -> date_reported

crime_total <- bind_cols(incidents, date_reported, dates_and_time)

crime_total
