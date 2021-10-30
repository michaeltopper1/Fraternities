## Purpose of script: exacts all the necessary NIBRS data.
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-05
##

library(tidyverse)
library(lubridate)

# gets the ori IDs needed -------------------------------------------------

closures <- readxl::read_excel("Data/closure_spreadsheet_final_2019.xlsx") %>% janitor::clean_names()

ori_schools_1 <- closures %>% 
  select(ori_9) %>% 
  pull()
ori_schools_2 <- closures %>% 
  select(ori_9_lindo) %>% pull()
ori_schools_3 <- closures %>% 
  select(ori_9_lindo2) %>% 
  pull()

orischools <- c(ori_schools_1, ori_schools_2, ori_schools_3) %>% 
  as_tibble() %>% 
  filter(!is.na(value)) %>% 
  distinct() %>% 
  pull()



# gets all of the administrative data and appending -----------------------


files_administrative <- map(list.files(path ="Data/nibrs/administrative/"), ~paste0("Data/nibrs/administrative/",.)) %>% 
  unlist

year <- 2014
for (file in files_administrative) {
  name <- paste0("admin_", year)
  admin <- read_rds(file) %>% 
    as_tibble() %>% 
    filter(ori %in% orischools)
  assign(name, admin)
  rm(admin)
  year <-  year + 1
}

admin_data <- map(mget(ls(pattern = "^admin")), ~.x %>% mutate(city_submissions = as.character(city_submissions))) %>% 
  reduce(bind_rows)
  
write_csv(admin_data, file = "Created Data/nibrs/admin_data.csv")
rm(list = ls(pattern = "^admin"))



# gettingn offense data and appending -------------------------------------


files_offense <- map(list.files("Data/nibrs/offense/"), ~paste0("Data/nibrs/offense/", .)) %>% 
  unlist()

year <- 2014
for (file in files_offense) {
    name <- paste0("offense_", year)
    admin <- read_rds(file) %>% 
      as_tibble() %>% 
      filter(ori %in% orischools)
    assign(name, admin)
    rm(admin)
    year <-  year + 1
}

offense_data <- map(mget(ls(pattern = "^offense")), ~.x %>% 
                      mutate(automatic_weapon_indicator_3 = as.character(automatic_weapon_indicator_3))) %>% 
  reduce(bind_rows)

write_csv(offense_data, file = "Created Data/nibrs/offense_data.csv")
rm(list = ls(pattern = "^offense"))



# getting offender data and appending -------------------------------------


files_offender <- map(list.files("Data/nibrs/offender/"), ~paste0("Data/nibrs/offender/", .)) %>% 
  unlist()

year <- 2014
for (file in files_offender) {
  name <- paste0("offender_", year)
  admin <- read_rds(file) %>% 
    as_tibble() %>% 
    filter(ori %in% orischools)
  assign(name, admin)
  rm(admin)
  year <-  year + 1
}

offender_data <- mget(ls(pattern = "^offender")) %>% 
  reduce(bind_rows)

write_csv(offender_data, file = "Created Data/nibrs/offender_data.csv")

rm(list = ls(pattern = "^off"))



# getting victim data and appending ---------------------------------------



victim_offender <- map(list.files("Data/nibrs/victim/"), ~paste0("Data/nibrs/victim/", .)) %>% 
  unlist()

year <- 2014
for (file in victim_offender) {
  name <- paste0("victim_", year)
  admin <- read_rds(file) %>% 
    as_tibble() %>% 
    filter(ori %in% orischools)
  assign(name, admin)
  rm(admin)
  year <-  year + 1
}

victim_data <- map(mget(ls(pattern = "^victim_2")), ~.x %>% 
                     mutate(ucr_offense_code_6 = as.character(ucr_offense_code_6),
                   ucr_offense_code_7 = as.character(ucr_offense_code_7))) %>% 
  reduce(bind_rows)

write_csv(victim_data, file = "Created Data/nibrs/victim_data.csv")

rm(list = ls(pattern = "^victim"))
