#############################################################################
##  This gets in all except the last page of the 2014 data from Louisiana  ##
#############################################################################


library(pdftools)
library(tabulizer)
library(tidyverse)

path <- "Data/campus_daily_crime_log/Louisiana State/2014crimelog.pdf"

louisiana <- pdf_text(path) %>% 
  str_split("\n") %>% 
  unlist() %>% 
  str_to_lower()

case_numbers <- louisiana %>% 
  str_detect("^case_number") %>% 
  which %>% magrittr::add(1)
description <- louisiana %>% 
  str_detect("^description") %>% 
  which %>% magrittr::add(1)

cases <- louisiana[case_numbers] %>% 
  as_tibble() %>% 
  separate(value, into = c('case_number', "date_reported", "date_occurred", "disposition"), sep = "\\s{2,}") %>% 
  extract(date_occurred, "date_occurred", "(\\d{1,2}/\\d{1,2}/\\d{1,4})") %>% 
  mutate(date_occurred = mdy(date_occurred), date_reported = mdy(date_reported))  %>% 
  select(-disposition)

incidents <- louisiana[description] %>% 
  as_tibble() %>% 
  separate(value, c("incident", "value"), "\\s{5,}") %>% 
  select(-value)

crime_2014 <- bind_cols(cases, incidents)

write_csv(crime_2014, file = "Data/campus_daily_crime_log/Louisiana State/2014.csv")

