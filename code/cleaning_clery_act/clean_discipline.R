## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-09-17
##

library(tidyverse)
library(readxl)

## This function cleans out the discpline files it has a couple of bells and whistles:
## reads in data, gets rid of the filter files that just have 1s
## changes colnames to match the file-type description (e.g., weapon17 is now oncampus_wearpon17)
## pivots the data so that we have a oncampus_weapon column and year in single columns
## @param regex is crime_2017/.{1,}discipline where crime_2017 is the name of the upper file. it will be either 2017 or 2019
clean_discipline <- function(file, regex, file_upper) {
  data <- read_excel(file) %>% janitor::clean_names() %>% 
    select(-starts_with("filter"))
  old_colnames <- colnames(data)
  crime_type <- file %>% 
    str_extract(regex) %>% 
    str_remove("discipline") %>% 
    str_remove(file_upper) %>% 
    str_to_lower()
  new_colnames <- paste0(crime_type, "_", old_colnames)
  colnames(data)[13:21] <- new_colnames[13:21]
  final_data <- data %>% 
    pivot_longer(cols = matches("\\d\\d$"),
                 names_pattern = "(.{1,})(\\d\\d)", names_to = c("crime", "year")) %>% 
    pivot_wider(names_from = crime, values_from = value) %>% 
    mutate(year = as.double(paste0("20",year)))
}


# extracting 2014-2016 ----------------------------------------------------


files_17 <- list.files("Data/clery_act_data/crime_2017/", pattern = "discipline") %>% 
  as.list()

files_17 <- map(files_17, ~paste0("Data/clery_act_data/crime_2017/", .)) 

discipline_17 <- map(files_17, ~clean_discipline(., "crime_2017/.{1,}discipline", file_upper = "crime_2017/")) %>% 
  reduce(bind_cols) %>% 
  rename(year = `year...13`) %>% 
  select(-matches("\\d{2}$")) ## need to omit the duplicate of the initial university name, number cols


# extracting 2017-2019 ----------------------------------------------------


files_20 <- list.files("Data/clery_act_data/crime_2020/", pattern = "discipline") %>% 
  as.list()

files_20 <- map(files_20, ~paste0("Data/clery_act_data/crime_2020/", .)) 


discipline_20 <- map(files_20, ~clean_discipline(., "crime_2020/.{1,}discipline", file_upper = "crime_2020/")) %>% 
  reduce(bind_cols) %>% 
  rename(year = `year...13`) %>% 
  select(-matches("\\d{2}$")) ## need to omit the duplicate of the initial university name, number cols



# appending  --------------------------------------------------------------


discipline <- bind_rows(discipline_17, discipline_20)

# gettting only schools ---------------------------------------------------

daily_crime <- read_csv("Created Data/xMaster_data_2021/yearly_panel.csv")
universities <- daily_crime %>% 
  distinct(university) %>% 
  pull()



discipline <- discipline %>% 
  filter(instnm %in% universities) %>% 
  select(-unitid_p, - branch, - address, - city, - state, - zip) %>% 
  group_by(year, instnm, sector_desc, sector_cd) %>% 
  summarize(across(everything(), ~sum(.,na.rm = T))) %>% 
  rename(university = instnm) 

write_csv(discipline, file = "Created Data/Clery_act_data/discipline.csv")
