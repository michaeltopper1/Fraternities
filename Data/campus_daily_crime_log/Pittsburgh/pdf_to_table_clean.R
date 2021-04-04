

library(tidyverse)
library(pdftools)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Pittsburgh/Media Log 2013.pdf"

pittsburgh_2013 <- pdf_text(path)

pittsburgh_2013 <- pittsburgh_2013 %>% 
  str_split('\n') %>% 
  unlist() %>% 
  str_trim() 

pittsburgh_2013 %>% 
  str_detect("Location:") %>% 
  which %>% magrittr::add(-2) -> incident
pittsburgh_2013 %>% 
  str_detect("Location:") %>% 
  which -> location

location <- pittsburgh_2013[location] %>% 
  str_split_fixed("\\s{3,}", n = 2) %>% 
  as_tibble() %>% 
  rename("location"= V2) %>% 
  select(location) 
incident <- pittsburgh_2013[incident] %>% 
  as_tibble() %>% 
  separate(value, into= c("incident", "other"), sep = "\\s{30,}", extra = "merge") %>% 
  extract(other, into = "date_reported", regex = "(.{3,9}\\s\\d{1,2},\\s\\d{2,4})", remove = F) %>% 
  extract(other, into = "time_reported", "(\\d\\d:\\d\\d)", remove = F) %>% 
  extract(other, "case_number", "(13-.{4,}$)")

pittsburgh <- bind_cols(incident, location) %>% 
  mutate(university = "University of Pittsburgh-Pittsburgh Campus", 
         time_occurred = NA, date_occurred = NA) %>% 
  mutate(date_reported = mdy(date_reported))

pittsburgh_2014_2019 <- read_csv("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Pittsburgh/2014_2019_Crime_Log.csv",
                                 col_names = F)
pittsburgh_2014_2019 <- pittsburgh_2014_2019[, 1:8]


pittsburgh_2014_2019 <- pittsburgh_2014_2019 %>% 
  janitor::clean_names() %>% 
  rename("university" = x1, "incident" = x3,
         "date_reported" = x4, "case_number" = x6, "location" = x8) %>% 
  select(-starts_with("x")) %>% 
  extract(date_reported, into = "time_reported", "(\\d\\d:\\d\\d)", remove = F) %>% 
  extract(date_reported, into = "date_reported", regex = "(.{3,9}\\s\\d{1,2},\\s\\d{2,4})", remove = F) %>% 
  mutate(date_reported = mdy(date_reported)) %>% 
  mutate(university = "University of Pittsburgh-Pittsburgh Campus", time_occurred = NA, date_occurred = NA)

pittsburgh <- bind_rows(pittsburgh, pittsburgh_2014_2019)


write_csv(pittsburgh, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/pittsburgh.csv")
