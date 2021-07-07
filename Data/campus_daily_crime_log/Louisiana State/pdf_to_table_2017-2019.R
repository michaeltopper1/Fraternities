#############################################################################
##  This gets in all 2017-2019 data from Louisiana  ##
#############################################################################


library(pdftools)
library(tabulizer)
library(tidyverse)

years <- c(2017:2019)
months <- c(1:12)

for (year in years) {
  for (month in months){
    file <- paste0("Data/campus_daily_crime_log/Louisiana State/", month, "_", year,".pdf")
    if (year ==2017 & month == 1) {
      louisiana <- pdf_text(file) %>% 
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
      
      crime<- bind_cols(cases, incidents)
    }
    else{
      louisiana <- pdf_text(file) %>% 
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
      
      crime_append<- bind_cols(cases, incidents)
      crime <- bind_rows(crime, crime_append)
    }
  }
}

write_csv(crime, "Data/campus_daily_crime_log/Louisiana State/2017_2019.csv")
