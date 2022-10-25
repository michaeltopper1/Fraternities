## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-10-24
##

library(tidyverse)

files_17 <- list.files("data/clery_act_data/crime_2017/", pattern = "arrest") %>% 
  as.list()

names_source <- map(files_17, ~str_remove(., pattern = "\\d{6}.xls")) %>% unlist()

files_17_paths <- map(files_17, ~paste0("data/clery_act_data/crime_2017/", .)) 

arrests_17 <- map_df(files_17_paths, ~readxl::read_excel(.) %>% janitor::clean_names(), .id = "source") %>% 
  mutate(source = case_when(
    source == "1" ~names_source[1],
    source == "2" ~ names_source[2],
    source =="3" ~names_source[3],
    source == "4" ~ names_source[4],
    source == "5"~ names_source[5]
  ))

arrests_17 <- arrests_17 %>% 
  select(-starts_with("filter")) %>% 
  pivot_longer(cols = matches("\\d\\d$"), names_to = "crime", 
               values_to = "count") %>% 
  separate(crime, into = c("crime", "year"), -2) %>% 
  filter(crime == "liquor") %>% 
  mutate(year = as.double(paste0("20", year)),
         crime = paste0(crime, "_", source)) 
  

x <- read_csv("created_data/clery_act/discipline.csv")

universities <- x %>% 
  distinct(university) %>% 
  pull(university)

arrests_17 <- arrests_17 %>% 
  select(-source) %>% 
  filter(instnm %in% universities) %>%
  pivot_wider(names_from = crime,
              values_from = count)

arrests_17 <- arrests_17 %>% 
  select(-unitid_p, - branch, - address, - city, - state, - zip) %>% 
  group_by(year, instnm, sector_desc, sector_cd) %>% 
  summarize(across(everything(), ~sum(.,na.rm = T))) %>% 
  rename(university = instnm) %>% 
  arrange(university) %>% 
  ungroup() 


# attach other years here -------------------------------------------------

files_20 <- list.files("data/clery_act_data/crime_2020/", pattern = "arrest") %>% 
  as.list()

names_source_20 <- map(files_17, ~str_remove(., pattern = "\\d{6}.xls")) %>% unlist()

files_20_paths <- map(files_20, ~paste0("data/clery_act_data/crime_2020/", .)) 

arrests_20 <- map_df(files_20_paths, ~readxl::read_excel(.) %>% janitor::clean_names(), .id = "source") %>% 
  mutate(source = case_when(
    source == "1" ~names_source_20[1],
    source == "2" ~ names_source_20[2],
    source =="3" ~names_source_20[3],
    source == "4" ~ names_source_20[4],
    source == "5"~ names_source_20[5]
  ))

arrests_20 <- arrests_20 %>% 
  select(-starts_with("filter")) %>% 
  pivot_longer(cols = matches("\\d\\d$"), names_to = "crime", 
               values_to = "count") %>% 
  separate(crime, into = c("crime", "year"), -2) %>% 
  filter(crime == "liquor") %>% 
  mutate(year = as.double(paste0("20", year)),
         crime = paste0(crime, "_", source)) 

arrests_20 <- arrests_20 %>% 
  select(-source) %>% 
  filter(instnm %in% universities) %>%
  pivot_wider(names_from = crime,
              values_from = count)

arrests_20 <- arrests_20 %>% 
  select(-unitid_p, - branch, - address, - city, - state, - zip) %>% 
  group_by(year, instnm, sector_desc, sector_cd) %>% 
  summarize(across(everything(), ~sum(.,na.rm = T))) %>% 
  rename(university = instnm) %>% 
  arrange(university) %>% 
  ungroup() 


# binding the two datas ---------------------------------------------------

arrests <- arrests_17 %>% 
  bind_rows(arrests_20) %>% 
  arrange(university) %>% 
  select(-matches("^m|^w"), -total) 

arrests <- arrests %>% 
  rowwise() %>% 
  mutate(liquor_total_arrests = sum(liquor_noncampusarrest, liquor_oncampusarrest, na.rm = T)) %>% 
  ungroup()


write_csv(arrests,"created_data/clery_act/arrests_liquor.csv")
