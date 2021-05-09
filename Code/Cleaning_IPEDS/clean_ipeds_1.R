## Purpose of script: clean first half of the IPEDS data. 
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-08
##

library(tidyverse)

## loading in data 
ipeds <- read_csv("Data/IPEDS/ipeds_1_5-8-21.csv")

## getting colnames into a tibble for easy manipulation
ipeds_names <- colnames(ipeds) %>% 
  as_tibble() %>% 
  rename("colnames" = value)

## cleaning columnnames and splitting into a tibble to easily manipulate
clean_names <- ipeds_names %>% 
  mutate(colnames = gsub("\\(not with family\\)", "-not with family-", colnames)) %>% 
  mutate(colnames = gsub("\\(year end\\)", "-year end-", colnames)) %>% 
  mutate(colnames = gsub("\\(GASB\\)", "-GASB-", colnames)) %>% 
  mutate(colnames = gsub("\\(with family\\)", "-with family-", colnames)) %>% 
  extract(colnames, "year_place", "(\\(.{1,}\\))", remove = F) %>% 
  extract(year_place, "year", "(\\d{4})", remove = F) %>% 
  mutate(colnames = str_replace(colnames, "\\(.{1,}\\)$", "")) %>% 
  mutate(colnames = str_replace(colnames, "\\d{1,}", "")) %>% 
  mutate(colnames = str_replace(colnames, "\\d{1,}", "")) %>% 
  mutate(colnames = gsub("-", "", colnames) %>% str_trim(.)) %>% 
  mutate(colnames = str_to_lower(colnames))

## setting columnnames
clean_names <- clean_names %>% 
  mutate(colnames = case_when(
    colnames == "total price for outofstate students living on campus" ~ "total_price_oncampus_oos",
    colnames == "total price for instate students living on campus" ~ "total_price_oncampus_is",
    colnames == "total price for indistrict students living on campus" ~ "total_price_oncampus_id",
    colnames == 'total price for indistrict students living off campus not with family'~ "total_price_offcampus_id",
    colnames == "total price for instate students living off campus not with family" ~"total_price_offcampus_is",
    colnames == "total price for outofstate students living off campus not with family" ~ "total_price_offcampus_oos",
    colnames == "total price for indistrict students living off campus with family" ~"total_price_offcampus_id_wfam",
    colnames == 'total price for instate students living off campus with family' ~"total_price_offcampus_is_wfam",
    colnames == "total price for outofstate students living off campus with family" ~ "total_price_offcampus_oos_wfam",
    colnames == "tuition and fees as a percent of core revenues gasb" ~ "perc_core_revenues_tuition_fees",
    colnames == "state appropriations as percent of core revenues  gasb" ~"perc_core_revenues_state_apro",
    colnames == "local appropriations as a percent of core revenues gasb" ~"perc_core_revenues_local_apro",
    colnames == "government grants and contracts as a percent of core revenues gasb" ~ "perc_core_revenues_gov_contracts",
    colnames == "private gifts  grants  and contracts as a percent of core revenues gasb" ~"perc_core_revenues_priv_gifts_grants",
    colnames == "investment return as a percent of core revenues gasb" ~ "perc_core_revenues_invest_return",
    colnames == "other revenues as a percent of core revenues gasb" ~ "perc_core_revenues_other",
    colnames == "endowment assets year end per fte enrollment gasb" ~ "endowment_assests",
    TRUE ~as.character(colnames)
  )) %>% 
  mutate(colnames =gsub("percent of undergraduate enrollment that are ", "perc_undergrad ", colnames)) %>% 
  mutate(colnames = gsub("percent of total enrollment that are", "perc_total", colnames)) %>% 
  mutate(colnames = gsub("percent of firsttime undergraduates", "perc_ftime_undergrad ", colnames)) %>%
  mutate(colnames = gsub("percent of fulltime firsttime undergraduates awarded", "perc_ftime_undergrad_award ", colnames)) %>%
  mutate(colnames = gsub("or african american", "", colnames)) 

## cleaning some column names and connecting them together
clean_names <- clean_names %>% 
  mutate(colnames = str_replace(colnames, "^percent", "frac")) %>% 
  mutate(colnames = str_replace(colnames, "^perc", "frac")) %>% 
  mutate(year = case_when(
    year == "1819" ~ "2018",
    year == "1718" ~ "2017",
    year == "1617" ~ "2016",
    year == "1516" ~ "2015",
    year == "1415" ~ "2014",
    is.na(year) ~ "",
    TRUE~as.character(year)
  )) %>% 
  mutate(colnames = paste0(colnames, " ", year)) 


## taking the cleaned columns from the tibble, and setting them to a vector
cleaned_colnames <- clean_names$colnames


## setting the colnames of the iPEDS data to the cleaned colnames
colnames(ipeds) <- cleaned_colnames  

## cleaning the colnames, and then going from wide to long. filtering out year 2013 as well
ipeds_long <- ipeds %>% 
  janitor::clean_names() %>% 
  select(-x) %>% 
  rename("university" = institution_name) %>% 
  pivot_longer(cols = -c(unitid, university), names_to = c(".value", "year"), names_pattern = "(.{1,})(\\d\\d\\d\\d)") %>% 
  filter(year != "2013")

## removing the hanging _ at the end of some of the names
colnames(ipeds_long) <- gsub("_$","", colnames(ipeds_long))


## cleaning final variables - turning fractions into actual fractions
ipeds_long <- ipeds_long %>% 
  mutate(control_of_institution = case_when(
    control_of_institution == 1 ~ "Public",
    control_of_institution == 2 ~ "Private not-for-profit",
    control_of_institution == 3 ~ "Private for-profit"
  )) %>% 
  mutate(level_of_institution = case_when(
    level_of_institution == 1 ~ "4-year",
    level_of_institution == 2 ~ "2-year",
    level_of_institution == 3 ~ "less than 2 years"
  )) %>% 
  mutate(across(starts_with("frac"), ~./100)) %>% 
  rename("unit_id" = unitid)

write_csv(ipeds_long, file = "Created Data/IPEDS/ipeds_1_cleaned.csv")