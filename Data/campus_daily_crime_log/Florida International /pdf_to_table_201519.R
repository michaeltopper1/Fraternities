
##################################################################################################################
##  This file reads in the pdf data from the 2nd page of everything from 2015 - 2019 for Florida International  ##
##################################################################################################################

library(tabulizer)
library(tidyverse)

## 2015_10 is when student appears
years <- c(2015:2019)
months <-
  c("01",
    "02",
    "03",
    "04",
    "05",
    "06",
    "07",
    "08",
    "09",
    "10",
    "11",
    "12")

## iterating through year and month
for (year in years) {
  print(year)
  for (month in months) {
    print(month)
    ## file path for the pdf - pasted together to make it loop
    file_path <-
      paste("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida International /",year,"_",month,".pdf",sep = "")
    ## I skip a few years that only have 1 page, not two pages
    if ((year == 2018 & month == "12") ) {
      next
    }
    if ((year == 2019 & month == "12")){
      next
    }
    if ((year == 2019 & month == "08")){
      next
    }
    if ((year == 2019 & month == "05")){
      next
    }
    if ((year == 2019 & month == "06")){
      next
    }
    ## extracting the tables from pages 2 and onward - I'll do pages 1 interactively
    florida_international <- extract_tables(file_path, pages = 2:get_n_pages(file = file_path))
    ## this next if-else statement essentially grabs each of the tables on each page and then concatenates them
    ## This is the first one so i can append the next
    if (year == 2015 & month == "01") {
      for (i in 1:length(florida_international)) {
        if (i == 1) {
          crime_table <- florida_international %>%
            pluck(i) %>%
            as_tibble()
        }
        else {
          new_table <- florida_international %>%
            pluck(i) %>%
            as_tibble()
          crime_table <- crime_table %>%
            bind_rows(new_table)
        }
      }
    }
    else{
      for (i in 1:length(florida_international)) {
        if (i == 1) {
          crime <- florida_international %>%
            pluck(i) %>%
            as_tibble() 
        }
        else {
          new_table <- florida_international %>%
            pluck(i) %>%
            as_tibble()
          crime <- crime %>%
            bind_rows(new_table)
        }
      }
      ## binding all of the next ones to the first one
      crime_table <- crime_table %>% 
        bind_rows(crime)
    }
  }
}

crime_table_2 <- crime_table

crime_table_2 <- crime_table_2 %>% 
  select(-V8, -V9)


save(crime_table_2, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida International /crime_201519.rda")
