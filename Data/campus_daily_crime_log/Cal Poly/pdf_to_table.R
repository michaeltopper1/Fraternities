library(tidyverse)
library(pdftools)
library(lubridate)
## year 2013 was very incomplete - ask heather about whether or not you should use this sort of data. 
years <- c(2014:2019)

for (year in years) {
  print(year)
  path <- paste("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cal Poly/Daily Crime Log ", year,".pdf",
                sep = "")
  cal_poly <- pdf_text(path)
  if (year == 2014){
    
    cal_poly %>% str_split("\n") %>% unlist() %>% str_to_lower() -> cal_poly
    
    cal_poly %>% 
      str_detect("date occurred from:") %>% which -> date_occurred_from
    cal_poly %>% 
      str_detect("general location:") %>% which -> location
    cal_poly %>% 
      str_detect("incident/offenses") %>%  which -> incident
    cal_poly %>% 
      str_detect("date reported") %>% which -> date_reported
    
    cal_poly[date_occurred_from] %>% 
      as_tibble() %>% 
      extract(value, into = "date_occurred", regex = "(\\d\\d/\\d\\d/\\d\\d)", remove = F) %>% 
      extract(value, into = "time_occurred", regex = "(\\d\\d:\\d\\d)") -> date_occurred
    
    cal_poly[location] %>% 
      as_tibble() %>% 
      separate(value, into = c("value", "location"), sep  = ":\\s{2,}") %>% 
      select(-value) -> location
    
    cal_poly[incident] %>% 
      as_tibble() %>% 
      separate(value, into = c("value", "incident"), sep = ":\\s{2,}") %>% 
      select(-value) -> incident
    cal_poly[date_reported] %>% 
      as_tibble() %>% 
      extract(value, into = "date_reported", regex = "(\\d\\d/\\d\\d/\\d\\d)", remove = F) %>% 
      extract(value, into = "time_reported", regex = "(\\d\\d:\\d\\d)", remove = F) %>% 
      separate(value, into = c("value", "case_number"), sep = "#:") %>% 
      select(date_reported, time_reported, case_number) %>% 
      mutate(case_number = str_trim(case_number)) -> date_reported
    
    cal_poly_final <- bind_cols(incident, date_reported, date_occurred, location)
    
  }
  else {
    
    cal_poly %>% str_split("\n") %>% unlist() %>% str_to_lower() -> cal_poly
    
    cal_poly %>% 
      str_detect("date occurred from:") %>% which -> date_occurred_from
    cal_poly %>% 
      str_detect("general location:") %>% which -> location
    cal_poly %>% 
      str_detect("incident/offenses") %>%  which -> incident
    cal_poly %>% 
      str_detect("date reported") %>% which -> date_reported
    
    cal_poly[date_occurred_from] %>% 
      as_tibble() %>% 
      extract(value, into = "date_occurred", regex = "(\\d\\d/\\d\\d/\\d\\d)", remove = F) %>% 
      extract(value, into = "time_occurred", regex = "(\\d\\d:\\d\\d)") -> date_occurred
    
    cal_poly[location] %>% 
      as_tibble() %>% 
      separate(value, into = c("value", "location"), sep  = ":\\s{2,}") %>% 
      select(-value) -> location
    
    cal_poly[incident] %>% 
      as_tibble() %>% 
      separate(value, into = c("value", "incident"), sep = ":\\s{2,}") %>% 
      select(-value) -> incident
    cal_poly[date_reported] %>% 
      as_tibble() %>% 
      extract(value, into = "date_reported", regex = "(\\d\\d/\\d\\d/\\d\\d)", remove = F) %>% 
      extract(value, into = "time_reported", regex = "(\\d\\d:\\d\\d)", remove = F) %>% 
      separate(value, into = c("value", "case_number"), sep = "#:") %>% 
      select(date_reported, time_reported, case_number) %>% 
      mutate(case_number = str_trim(case_number)) -> date_reported
    
    cal_poly_append <- bind_cols(incident, date_reported, date_occurred, location)
    cal_poly_final <- cal_poly_final %>% 
      bind_rows(cal_poly_append)
  }
}

cal_poly <- cal_poly_final %>% 
  mutate(date_occurred = mdy(date_occurred)) %>% 
  mutate(date_reported = mdy(date_reported)) %>% 
  mutate(university ="California Polytechnic State University-San Luis Obispo")

write_csv(cal_poly, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/cal_poly.csv")
