## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-21
##

library(tidyverse)
library(readxl)
library(lubridate)

closure_spreadsheet <- read_xlsx("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/closure_spreadsheet_final_2019.xlsx", 
                                 sheet = "closure_spreadsheet_main") %>% janitor::clean_names()

## grabs the list of universities that have moratoria 
universities <- closure_spreadsheet %>% 
  distinct(university) %>% 
  pull()

directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/clery_act_data/Clery_act_sexual"
setwd(directory)
files <- list.files(directory)

## finds the indices which each type of file exists
files %>% str_detect("^noncampus") %>% which -> noncampus_files
files %>% str_detect("^oncampus") %>% which -> oncampus_files
files %>% str_detect("^residencehall") %>% which -> residencehall_files
files %>% str_detect("^publicpropertycrime") %>% which -> publicprop_files

##### Rapes
## getting the rapes non-campus
rape_noncampus <- map(files[noncampus_files], ~read_xls(.) %>% janitor::clean_names() %>% 
      select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
             starts_with("rape")) %>% 
      filter(instnm %in% universities) %>% 
      pivot_longer(cols = starts_with("rape"), names_to = "year", names_pattern = "(\\d\\d)",
                   values_to = "rape_noncampus") %>% 
      mutate(year = paste("20", year, sep = "")) %>% 
      rename("university" = instnm)) %>% 
  reduce(bind_rows)

## getting the rapes on-campus
rape_oncampus <- map(files[oncampus_files], ~read_xls(.) %>% janitor::clean_names() %>% 
      select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
             starts_with("rape")) %>% 
      filter(instnm %in% universities) %>% 
      pivot_longer(cols = starts_with("rape"), names_to = "year", names_pattern = "(\\d\\d)",
                   values_to = "rape_oncampus") %>% 
      mutate(year = paste("20", year, sep = "")) %>% 
      rename("university" = instnm)) %>% 
  reduce(bind_rows)

## getting the residence hall rapes
rape_reshall <- map(files[residencehall_files], ~read_xls(.) %>% janitor::clean_names() %>% 
      select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
             starts_with("rape")) %>% 
      filter(instnm %in% universities) %>% 
      pivot_longer(cols = starts_with("rape"), names_to = "year", names_pattern = "(\\d\\d)",
                   values_to = "rape_reshall") %>% 
      mutate(year = paste("20", year, sep = "")) %>% 
      rename("university" = instnm)) %>% 
  reduce(bind_rows)
## getting the public prop   rapes
rape_publicprop <- map(files[publicprop_files], ~read_xls(.) %>% janitor::clean_names() %>% 
                      select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
                             starts_with("rape")) %>% 
                      filter(instnm %in% universities) %>% 
                      pivot_longer(cols = starts_with("rape"), names_to = "year", names_pattern = "(\\d\\d)",
                                   values_to = "rape_publicprop") %>% 
                      mutate(year = paste("20", year, sep = "")) %>% 
                      rename("university" = instnm)) %>% 
  reduce(bind_rows)

###### Fondling
## getting the fondling non-campus
fondle_noncampus <- map(files[noncampus_files], ~read_xls(.) %>% janitor::clean_names() %>% 
      select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
             starts_with("fondl")) %>% 
      filter(instnm %in% universities) %>% 
      pivot_longer(cols = starts_with("fondl"), names_to = "year", names_pattern = "(\\d\\d)",
                   values_to = "fondl_noncampus") %>% 
      mutate(year = paste("20", year, sep = "")) %>% 
      rename("university" = instnm)) %>% 
  reduce(bind_rows)

## getting the fondling on-campus
fondle_oncampus <- map(files[oncampus_files], ~read_xls(.) %>% janitor::clean_names() %>% 
      select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
             starts_with("fondl")) %>% 
      filter(instnm %in% universities) %>% 
      pivot_longer(cols = starts_with("fondl"), names_to = "year", names_pattern = "(\\d\\d)",
                   values_to = "fondl_oncampus") %>% 
      mutate(year = paste("20", year, sep = "")) %>% 
      rename("university" = instnm)) %>% 
  reduce(bind_rows)

## getting the fondling hall rapes
fondle_reshall <- map(files[residencehall_files], ~read_xls(.) %>% janitor::clean_names() %>% 
      select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
             starts_with("fondl")) %>% 
      filter(instnm %in% universities) %>% 
      pivot_longer(cols = starts_with("fondl"), names_to = "year", names_pattern = "(\\d\\d)",
                   values_to = "fondl_reshall") %>% 
      mutate(year = paste("20", year, sep = "")) %>% 
      rename("university" = instnm)) %>% 
  reduce(bind_rows)

## getting the fondling public prop
fondle_publicprop <- map(files[publicprop_files], ~read_xls(.) %>% janitor::clean_names() %>% 
                        select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
                               starts_with("fondl")) %>% 
                        filter(instnm %in% universities) %>% 
                        pivot_longer(cols = starts_with("fondl"), names_to = "year", names_pattern = "(\\d\\d)",
                                     values_to = "fondl_publicprop") %>% 
                        mutate(year = paste("20", year, sep = "")) %>% 
                        rename("university" = instnm)) %>% 
  reduce(bind_rows)

#### Statutory Rape
## getting the stat rapes non-campus
statr_noncampus <- map(files[noncampus_files], ~read_xls(.) %>% janitor::clean_names() %>% 
      select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
             starts_with("statr")) %>% 
      filter(instnm %in% universities) %>% 
      pivot_longer(cols = starts_with("statr"), names_to = "year", names_pattern = "(\\d\\d)",
                   values_to = "statr_noncampus") %>% 
      mutate(year = paste("20", year, sep = "")) %>% 
      rename("university" = instnm)) %>% 
  reduce(bind_rows)

## getting the stat rapes on-campus
statr_oncampus <- map(files[oncampus_files], ~read_xls(.) %>% janitor::clean_names() %>% 
      select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
             starts_with("statr")) %>% 
      filter(instnm %in% universities) %>% 
      pivot_longer(cols = starts_with("statr"), names_to = "year", names_pattern = "(\\d\\d)",
                   values_to = "statr_oncampus") %>% 
      mutate(year = paste("20", year, sep = "")) %>% 
      rename("university" = instnm)) %>% 
  reduce(bind_rows) 

## getting the stat rapes hall rapes
statr_reshall <- map(files[residencehall_files], ~read_xls(.) %>% janitor::clean_names() %>% 
      select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
             starts_with("statr")) %>% 
      filter(instnm %in% universities) %>% 
      pivot_longer(cols = starts_with("statr"), names_to = "year", names_pattern = "(\\d\\d)",
                   values_to = "statr_reshall") %>% 
      mutate(year = paste("20", year, sep = "")) %>% 
      rename("university" = instnm)) %>% 
  reduce(bind_rows)

## getting thestat rapes public property
statr_publicprop <- map(files[publicprop_files], ~read_xls(.) %>% janitor::clean_names() %>% 
                         select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
                                starts_with("statr")) %>% 
                         filter(instnm %in% universities) %>% 
                         pivot_longer(cols = starts_with("statr"), names_to = "year", names_pattern = "(\\d\\d)",
                                      values_to = "statr_publicprop") %>% 
                         mutate(year = paste("20", year, sep = "")) %>% 
                         rename("university" = instnm)) %>% 
  reduce(bind_rows)


## This binds together all of the different data frames
clery_rapes <- fondle_noncampus %>% 
  left_join(fondle_oncampus) %>% 
  left_join(fondle_reshall) %>% 
  left_join(rape_noncampus) %>% 
  left_join(rape_oncampus) %>% 
  left_join(rape_reshall) %>% 
  left_join(statr_noncampus) %>% 
  left_join(statr_oncampus) %>% 
  left_join(statr_reshall) %>% 
  left_join(fondle_publicprop) %>% 
  left_join(rape_publicprop) %>% 
  left_join(statr_publicprop)


## this takes the sum of each of the four categories while stripping the NAs
clery_rapes <- clery_rapes %>% 
  rowwise %>% 
  mutate(fondl_total = sum(fondl_noncampus, fondl_oncampus, fondl_publicprop, na.rm = T)) %>% 
  mutate(rape_total = sum(rape_noncampus, rape_oncampus, rape_publicprop, na.rm = T)) %>% 
  mutate(statr_total = sum(statr_noncampus, statr_oncampus, statr_publicprop, na.rm = T)) %>% 
  mutate(sexual_assault_total = sum(rape_total, statr_total, fondl_total, na.rm = T))

## This sums up the counts so that I no longer have branchs of each school
clery_rapes <- clery_rapes %>% 
  mutate(year = as.double(year)) %>% 
  group_by(university, year) %>% 
  summarize(sexual_assault_total_cl = sum(sexual_assault_total, na.rm = T), rape_total = sum(rape_total, na.rm = T),
            statr_total_cl = sum(statr_total, na.rm = T), fondl_total = sum(fondl_total, na.rm = T),
            rape_reshall = sum(rape_reshall, na.rm = T), rape_oncampus = sum(rape_oncampus, na.rm = T),
            rape_noncampus = sum(rape_noncampus, na.rm= T), fondl_noncampus = sum(fondl_noncampus, na.rm = T),
            fondl_oncampus = sum(fondl_oncampus, na.rm = T), fondl_reshall = sum(fondl_reshall, na.rm = T),
            statr_noncampus = sum(statr_noncampus, na.rm = T), statr_oncampus = sum(statr_oncampus, na.rm = T),
            statr_reshall = sum(statr_reshall, na.rm = T),
            fondl_total_cl = sum(fondl_total, na.rm = T)) 

write_csv(clery_rapes, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Clery_act_data/sexual_assault_panel_final.csv")
