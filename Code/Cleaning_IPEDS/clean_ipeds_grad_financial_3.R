## Purpose of script: Cleans the grad rate and financial aid variables
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-19
##

library(tidyverse)



## getting the university names
source("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Code/Cleaning_raw_data_files/clean_excel_closure_dates.R")



path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/IPEDS/graduation_rate_financial_aid/Data_2-19-2021.csv"

## getting only the universities I have in my sample
ipeds_grad_rate_financial <- read_csv(path) %>% 
  janitor::clean_names() %>% 
  filter(institution_name %in% closure_table_round$university)

ipeds_financial_aid <- ipeds_grad_rate_financial %>% 
  select(unit_id, institution_name, starts_with("total")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = c(".value", "year"),
               names_sep = "sfa") %>% 
  extract(year, "year", "(\\d\\d\\d\\d)") %>% 
  mutate(year = case_when(
    year == "1819"~ 2018,
    year == "1718" ~2017,
    year == "1617" ~2016,
    year == "1516" ~ 2015,
    year == "1415" ~2014,
    year == "1314" ~2013,
    year == "1213" ~2012
  ))  %>% 
  filter(year >= 2013)


ipeds_grad_rate <- ipeds_grad_rate_financial %>% 
  select(unit_id, institution_name, starts_with("grad")) %>% 
  pivot_longer(cols = -c(unit_id, institution_name), names_to = c(".value", "year"),
               names_sep = "drvgr") %>% 
  extract(year, "year", "(\\d\\d\\d\\d)") %>% 
  mutate(year = as.double(year)) 

ipeds_grad_financial <- ipeds_grad_rate %>% 
  left_join(ipeds_financial_aid)

write_csv(ipeds_grad_financial, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/IPEDS/unappended/ipeds_grad_financial.csv")
