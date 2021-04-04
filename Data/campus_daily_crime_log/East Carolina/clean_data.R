

library(tidyverse)
library(pdftools)
library(lubridate)
library(readxl)

years <- c(2014:2019)

for (year in years) {
  path <- paste("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/East Carolina/crime_",year,".XLSX",
                sep = "")
  if (year == 2014) {
    east_carolina <- read_xlsx(path)
  }
  else {
    next_year <- read_xlsx(path)
    east_carolina <- east_carolina %>% 
      bind_rows(next_year)
  }
}

east_carolina <- east_carolina %>% 
  janitor::clean_names() %>% 
  select(-starts_with("x")) %>% 
  filter(across(everything(), ~!is.na(.))) 



## a few dates couldn't parse correctly, but that's ok - these were generally missing dates
east_carolina <- east_carolina %>% 
  rename("date_reported" = date_time_reported, 
         "date_occurred" = date_time_occurred,
         "location" = general_location) %>% 
  select(-disposition) %>% 
  extract(date_reported, "time_reported", "(\\d\\d\\d\\d\\s{0,1}hr)", remove= F) %>% 
  extract(time_reported, "time_reported", "(\\d\\d\\d\\d)") %>% 
  extract(date_reported, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d{2,4})") %>% 
  extract(date_occurred, "time_occurred", "(\\d{4}\\s{0,1}hr)", remove = F) %>% 
  extract(date_occurred, "date_occurred", "(\\d{1,2}/\\d{1,2}/\\d{2,4})") %>% 
  extract(time_occurred, "time_occurred", "(\\d\\d\\d\\d)") %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(across(starts_with("time"), ~format(strptime(., "%H%M"), format= "%H:%M"))) %>% 
  mutate(university = "East Carolina University")

east_carolina <- east_carolina %>% 
  group_by(across(-incident)) %>% 
  summarize(incident = paste(incident, collapse = " ")) %>% 
  ungroup()

write_csv(east_carolina, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/eastcarolina.csv")
