## Purpose of script: extracts all the necessary NIBRS data.
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-05
##

library(tidyverse)
library(lubridate)

# gets the ori IDs needed -------------------------------------------------
## These ori IDs are from the Lindo paper. I first went through and gathered those that are in NIBRS
## Then found the ori IDs that lindo used to connect.
## I also added in schools that are never treated and never had a university death using academic calendar file.

closures <- readxl::read_excel("data/closure_spreadsheet_final_2019.xlsx") %>% janitor::clean_names()
academic_calendars <- readxl::read_excel("data/academic_calendars_ori.xlsx")

ori_schools_1 <- closures %>% 
  select(ori_9) %>% 
  pull()
ori_schools_2 <- closures %>% 
  select(ori_9_lindo) %>% pull()
ori_schools_3 <- closures %>% 
  select(ori_9_lindo2) %>% 
  pull()
ori_schools_ac <- academic_calendars %>% distinct(ori) %>% pull()
orischools <- c(ori_schools_1, ori_schools_2, ori_schools_3, ori_schools_ac) %>% 
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
  
write_csv(admin_data, file = "created_data/nibrs/admin_data.csv")
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

write_csv(offense_data, file = "created_data/nibrs/offense_data.csv")
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

write_csv(offender_data, file = "created_data/nibrs/offender_data.csv")

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

write_csv(victim_data, file = "created_data/nibrs/victim_data.csv")

rm(list = ls(pattern = "^victim"))


# getting group B arrest data and appending -------------------------------

arrest_b <- map(list.files("data/nibrs/group_b_arrest/"), ~paste0("data/nibrs/group_b_arrest/",.)) %>% 
  unlist()

year <- 2014
for (file in arrest_b) {
  name <- paste0("group_b_", year)
  admin <- read_rds(file) %>% 
    as_tibble() %>% 
    filter(ori %in% orischools)
  assign(name, admin)
  rm(admin)
  year <-  year + 1
}

group_b_data <- map_df(mget(ls(pattern = "^group_b")), ~.x)

write_csv(group_b_data, file = "created_data/nibrs/group_b_arrests.csv")

rm(list = ls(pattern = "^group"))
        