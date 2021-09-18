## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-09-17
##

library(tidyverse)
library(readxl)



clean_crime <- function(file, regex, file_upper) {
  data <- read_excel(file) %>% janitor::clean_names() %>% 
    select(-starts_with("filter"))
  old_colnames <- colnames(data)
  crime_type <- file %>% 
    str_extract(regex) %>% 
    str_remove(file_upper) %>% 
    str_remove("crime") %>% 
    str_to_lower()
  new_colnames <- paste0(crime_type, "_", old_colnames)
  colnames(data)[13:48] <- new_colnames[13:48]
  final_data <- data %>% 
    pivot_longer(cols = matches("\\d\\d$"),
                 names_pattern = "(.{1,})(\\d\\d)", names_to = c("crime", "year")) %>% 
    pivot_wider(names_from = crime, values_from = value) %>% 
    mutate(year = as.double(paste0("20",year)))
}


# extracting 2014-2017 ----------------------------------------------------

files_17 <- list.files(path = "Data/clery_act_data/crime_2017/", pattern = "crime")

files_17 <- map(files_17, ~paste0("Data/clery_act_data/crime_2017/", .)) 

crime_17 <- map(files_17, ~clean_crime(., "crime_2017/.{1,}crime", file_upper = "crime_2017/")) %>% 
  reduce(bind_cols) %>% 
  rename(year = `year...13`) %>% 
  select(-matches("\\d{2}$"))



# extracting 2017-2019 ----------------------------------------------------


files_20 <- list.files("Data/clery_act_data/crime_2020/", pattern = "crime") %>% 
  as.list()

files_20 <- map(files_20, ~paste0("Data/clery_act_data/crime_2020/", .)) 


crime_20 <- map(files_20, ~clean_crime(., "crime_2020/.{1,}crime", file_upper = "crime_2020/")) %>% 
  reduce(bind_cols) %>% 
  rename(year = `year...13`) %>% 
  select(-matches("\\d{2}$")) ## need to omit the duplicate of the initial university name, number cols


# appending ---------------------------------------------------------------

crime <- bind_rows(crime_17, crime_20)

daily_crime <- read_csv("Created Data/xMaster_data_2021/yearly_panel.csv")
universities <- daily_crime %>% 
  distinct(university) %>% 
  pull()

crime <- crime %>% 
  filter(instnm %in% universities) %>% 
  select(-unitid_p, - branch, - address, - city, - state, - zip) %>% 
  group_by(year, instnm, sector_desc, sector_cd) %>% 
  summarize(across(everything(), ~sum(.,na.rm = T))) %>% 
  rename(university = instnm) 



# saving ------------------------------------------------------------------

write_csv(crime, file = "Created Data/Clery_act_data/crime.csv")
