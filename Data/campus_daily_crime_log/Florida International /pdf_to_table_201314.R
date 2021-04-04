##################################################################################################################
##  This file reads in the pdf data  (including first page) of everything from 2015 - 2019 for Florida International  ##
##################################################################################################################


library(tabulizer)
library(tidyverse)

path <-
  "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida International /2014_03.pdf"
path_2 <-
  "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida International /2013_02.pdf"
florida_international <- extract_tables(path)

for (i in 1:length(florida_international)) {
  if (i == 1) {
    crime_table <- florida_international %>%
      pluck(1) %>%
      as_tibble() %>%
      janitor::row_to_names(row_number = 1)
    table_names <- colnames(crime_table)
  }
  else {
    new_table <- florida_international %>%
      pluck(i) %>%
      as_tibble()
    colnames(new_table) <- table_names
    crime_table <- crime_table %>%
      bind_rows(new_table)
  }
}

## 2015_10 is when student appears
years <- c(2013:2014)
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

for (year in years) {
  print(year)
  for (month in months) {
    print(month)
    file_path <-
      paste("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida International /",year,"_",month,".pdf",sep = "")
    florida_international <- extract_tables(file_path)
    if (year == 2013 & month == "01") {
      for (i in 1:length(florida_international)) {
        if (i == 1) {
          crime_table <- florida_international %>%
            pluck(i) %>%
            as_tibble() %>%
            janitor::row_to_names(row_number = 1)
          table_names <- colnames(crime_table)
        }
        else {
          new_table <- florida_international %>%
            pluck(i) %>%
            as_tibble()
          colnames(new_table) <- table_names
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
          as_tibble() %>%
          janitor::row_to_names(row_number = 1)
        table_names <- colnames(crime_table)
      }
        else {
        new_table <- florida_international %>%
          pluck(i) %>%
          as_tibble()
        colnames(new_table) <- table_names
        crime <- crime %>%
          bind_rows(new_table)
        }
      }
      crime_table <- crime_table %>% 
        bind_rows(crime)
    }
  }
}

crime_1314 <- crime_table
save(crime_1314, file =  "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida International /crime_201314.rda")
