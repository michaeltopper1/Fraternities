## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-12
##

library(tidyverse)
library(readxl)


closure_spreadsheet <- read_xlsx("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/closure_spreadsheet_final_2019.xlsx", 
                                 sheet = "closure_spreadsheet_main") %>% janitor::clean_names()

## grabs the list of universities that have moratoria 
universities <- closure_spreadsheet %>% 
  distinct(university) %>% 
  pull()


setwd("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/clery_act_data/Clery_act_files_used")
files <- list.files()

## finds the indices which each type of file exists
files %>% str_detect("^noncampus") %>% which -> noncampus_files
files %>% str_detect("^oncampus") %>% which -> oncampus_files
files %>% str_detect("^residencehall") %>% which -> residencehall_files
files %>% str_detect("^publicprop") %>% which -> publicprop_files

oncampus_dis_liquor <- map(files[oncampus_files], ~read_xls(.) %>% janitor::clean_names() %>% 
                      select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
                             starts_with("liquor")) %>% 
                      filter(instnm %in% universities) %>% 
                      pivot_longer(cols = starts_with("liquor"), names_to = "year", names_pattern = "(\\d\\d)",
                                   values_to = "liquor_violations_campus") %>% 
                      mutate(year = paste("20", year, sep = "")) %>% 
                      rename("university" = instnm)) %>% 
  reduce(bind_rows)
reshall_dis_liquor <- map(files[residencehall_files], ~read_xls(.) %>% janitor::clean_names() %>% 
                             select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
                                    starts_with("liquor")) %>% 
                             filter(instnm %in% universities) %>% 
                             pivot_longer(cols = starts_with("liquor"), names_to = "year", names_pattern = "(\\d\\d)",
                                          values_to = "liquor_violations_reshall") %>% 
                             mutate(year = paste("20", year, sep = "")) %>% 
                             rename("university" = instnm)) %>% 
  reduce(bind_rows)
publicprop_dis_liquor <- map(files[publicprop_files], ~read_xls(.) %>% janitor::clean_names() %>% 
                             select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
                                    starts_with("liquor")) %>% 
                             filter(instnm %in% universities) %>% 
                             pivot_longer(cols = starts_with("liquor"), names_to = "year", names_pattern = "(\\d\\d)",
                                          values_to = "liquor_violations_publicprop") %>% 
                             mutate(year = paste("20", year, sep = "")) %>% 
                             rename("university" = instnm)) %>% 
  reduce(bind_rows)

noncampus_dis_liquor <- map(files[noncampus_files], ~read_xls(.) %>% janitor::clean_names() %>% 
                       select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
                              starts_with("liquor")) %>% 
                       filter(instnm %in% universities) %>% 
                       pivot_longer(cols = starts_with("liquor"), names_to = "year", names_pattern = "(\\d\\d)",
                                    values_to = "liquor_violations_offcampus") %>% 
                       mutate(year = paste("20", year, sep = "")) %>% 
                       rename("university" = instnm)) %>% 
  reduce(bind_rows)


## drugs
oncampus_dis_drug <- map(files[oncampus_files], ~read_xls(.) %>% janitor::clean_names() %>% 
                           select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
                                  starts_with("drug")) %>% 
                           filter(instnm %in% universities) %>% 
                           pivot_longer(cols = starts_with("drug"), names_to = "year", names_pattern = "(\\d\\d)",
                                        values_to = "drug_violations_campus") %>% 
                           mutate(year = paste("20", year, sep = "")) %>% 
                           rename("university" = instnm)) %>% 
  reduce(bind_rows)
noncampus_dis_drug <- map(files[noncampus_files], ~read_xls(.) %>% janitor::clean_names() %>% 
                            select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
                                   starts_with("drug")) %>% 
                            filter(instnm %in% universities) %>% 
                            pivot_longer(cols = starts_with("drug"), names_to = "year", names_pattern = "(\\d\\d)",
                                         values_to = "drug_violations_offcampus") %>% 
                            mutate(year = paste("20", year, sep = "")) %>% 
                            rename("university" = instnm)) %>% 
  reduce(bind_rows)
reshall_dis_drug <- map(files[residencehall_files], ~read_xls(.) %>% janitor::clean_names() %>% 
                           select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
                                  starts_with("drug")) %>% 
                           filter(instnm %in% universities) %>% 
                           pivot_longer(cols = starts_with("drug"), names_to = "year", names_pattern = "(\\d\\d)",
                                        values_to = "drug_violations_reshall") %>% 
                           mutate(year = paste("20", year, sep = "")) %>% 
                           rename("university" = instnm)) %>% 
  reduce(bind_rows)
publicprop_dis_drug <- map(files[publicprop_files], ~read_xls(.) %>% janitor::clean_names() %>% 
                           select(unitid_p, instnm, branch, address, city, state, zip, men_total, women_total, total,
                                  starts_with("drug")) %>% 
                           filter(instnm %in% universities) %>% 
                           pivot_longer(cols = starts_with("drug"), names_to = "year", names_pattern = "(\\d\\d)",
                                        values_to = "drug_violations_publicprop") %>% 
                           mutate(year = paste("20", year, sep = "")) %>% 
                           rename("university" = instnm)) %>% 
  reduce(bind_rows)


clery_discipline <- noncampus_dis_drug %>% 
  left_join(oncampus_dis_drug) %>% 
  left_join(reshall_dis_drug) %>% 
  left_join(publicprop_dis_drug) %>% 
  left_join(noncampus_dis_liquor) %>% 
  left_join(oncampus_dis_liquor) %>% 
  left_join(reshall_dis_liquor) %>% 
  left_join(publicprop_dis_liquor)

clery_discipline <- clery_discipline %>% 
  rowwise %>% 
  mutate(liquor_violation_total = sum(liquor_violations_offcampus, liquor_violations_campus, liquor_violations_reshall,
                                      liquor_violations_publicprop, na.rm = T)) %>% 
  mutate(drug_violation_total = sum(drug_violations_campus, drug_violations_offcampus, drug_violations_publicprop,
                                    drug_violations_reshall, na.rm = T))

clery_discipline <- clery_discipline %>% 
  group_by(year, university) %>% 
  mutate(year = as.double(year)) %>% 
  summarize(liquor_violations_offcampus = sum(liquor_violations_offcampus, na.rm = T),
            liquor_violations_campus = sum(liquor_violations_campus, na.rm = T),
            liquor_violations_reshall = sum(liquor_violations_reshall, na.rm = T),
            liquor_violations_publicprop = sum(liquor_violations_publicprop, na.rm = T),
            drug_violations_offcampus = sum(drug_violations_offcampus, na.rm= T),
            drug_violations_campus = sum(drug_violations_campus, na.rm = T),
            drug_violations_reshall = sum(drug_violations_reshall, na.rm = T),
            drug_violations_publicprop = sum(drug_violations_publicprop, na.rm = T),
            liquor_violation_total_cl = sum(liquor_violation_total, na.rm = T),
            drug_violation_total_cl = sum(drug_violation_total, na.rm = T))

write_csv(clery_discipline, 
          file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Clery_act_data/drug_alcohol_panel_final.csv")
