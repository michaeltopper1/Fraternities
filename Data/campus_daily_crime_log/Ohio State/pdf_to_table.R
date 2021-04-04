## Purpose of script:
## ALERT:: need to take out the redacted sheets and manually enter them
## PDF_TEXT WORKS ONLY WITHOUT REDACTED PAGES!!
## Author: Michael Topper
##
## Date Last Edited: 2021-02-09
##

library(tidyverse)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Ohio State/2014 Daily Log_Redacted.xlsx"

ohio_2014 <- readxl::read_xlsx(path) %>% 
  janitor::clean_names()

incidents <- ohio_2014 %>% 
  filter(!is.na(incident_type))


ohio_2014 %>% 
  pull(incident_type) -> incidents 

incidents %>% 
  str_detect("^[^N][^A]") %>% 
  which %>% magrittr::add(-1)-> ohio_reported_indices


ohio_2014 %>% 
  pull(reported) -> reported
reported[ohio_reported_indices] %>% 
  as_tibble() %>% 
  rename("date_reported" = value) %>% 
  bind_cols(incidents) %>% 
  relocate(date_reported, incident_type) %>% View()


ohio <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Ohio State/2014 Daily Log_Redacted.pdf"
library(pdftools)
library(tidyverse)
ohio <- pdf_text(ohio)
ohio[[48]] %>% 
  str_split('\n') %>% 
  unlist() %>% 
  str_trim()

ohio_44 <- "/Users/michaeltopper/Desktop/ohio_2014_44.pdf"
ohio_44 <- pdf_text(ohio_44)
ohio_44 %>% 
  str_split('\n') %>% 
  unlist() %>% 
  str_trim() %>% 
  head(20)
