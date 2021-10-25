## Purpose of script: Gives matching word table and top categories for each match figure.
##
## Author: Michael Topper
##
## Date Last Edited: 2021-4-30
##

library(tidyverse)
library(glue)
library(ifc)

## getting in the data that I matched with 
# source("Code/Cleaning_crime_log_files/append_daily_crime_logs_1.R")


# identifier - pull from append_daily_crime_logs_1 ------------------------

appended_crime_logs <-  read_csv("Created Data/xMaster_data_2021/appended_crime_logs.csv") %>% 
  filter(university %in% ifc::moratorium_schools())

alcohol_identifiers <- "alcohol|dwi|intox|drink|dui|drunk|liquor|driving under the influence|dip|abcc|underage|dwi|underage|pula|owi|mip|under age|beer|wine|booze|minor in possession|ovi" ## got rid of disorderly conduct.
sexual_assault_identifiers <- "sex|rape|fondling|fondle" 
drug_identifiers <- "drug|narcotic|marijuana|heroin|overdose|cocaine|controlled substance"
theft_identifiers <- "larceny|theft|shoplifting|pocket-picking|steal|shop lifting" ##using nibrs
robbery_burglary_identifiers <- "robbery|burglary|unlawful entry|breaking and entering"
alcohol_identifiers_strict <- "alcohol|dwi|intox|drink|dui|drunk|liquor|driving under the influence|dip|abcc|underage|beverage|dwi|underage|container|pula|owi|mip|under age|minor in possession|ovi" ## getting rid of possesion
noise_violation_identifier <- "noise|loud"
rape_identifier <- "rape"

## table of words to match on
alcohol_words <- alcohol_identifiers %>% 
  str_replace_all("\\|", ", ")
sexual_assault_words <-  sexual_assault_identifiers %>% 
  str_replace_all("\\|", ", ")
drug_offense_words <-  drug_identifiers %>% 
  str_replace_all("\\|", ", ")
robbery_burglary_words <- robbery_burglary_identifiers %>% 
  str_replace_all("\\|", ", ")
theft_words <- theft_identifiers %>% 
  str_replace_all("\\|", ", ")
words <- list(sexual_assault_words, alcohol_words, drug_offense_words,  robbery_burglary_words)  %>% 
  unlist()
categories <- c("Sexual Assault", "Alcohol Violations", "Drug Offense",
                "Robbery/Burglary")
matching_table <- tibble("Outcome" = categories, "Words to Match" = words)


top_drug_alcohol <- appended_crime_logs %>% 
  filter(drug_offense == 1 & alcohol_offense == 1) %>% 
  count(incident, sort = T)  %>% 
  head(30) %>% 
  mutate(incident = glue("({n}) {incident}")) %>% 
  select(incident) %>% rename("Drug and Alcohol" = incident)

top_sexual_assault <- appended_crime_logs %>% 
  filter(sexual_assault == 1) %>% 
  count(incident, sort = T) %>% 
  head(30) %>% 
  mutate(incident = glue("({n}) {incident}")) %>% 
  select(incident) %>% rename("Sexual Assault" = incident)

top_alcohol_offense <- appended_crime_logs %>% 
  filter(alcohol_offense == 1) %>% 
  count(incident, sort = T) %>% 
  mutate(incident = glue("({n}) {incident}")) %>% 
  head(30) %>% select(incident) %>% rename("Alcohol Offense" = incident)

top_drug_offense <- appended_crime_logs %>% 
  filter(drug_offense == 1) %>% 
  count(incident, sort = T) %>% 
  mutate(incident = glue("({n}) {incident}")) %>% 
  head(30) %>%  select(incident) %>% rename("Drug Offense" = incident)


top_theft <- appended_crime_logs %>% 
  filter(theft == 1) %>% 
  count(incident, sort = T) %>% 
  mutate(incident = glue("({n}) {incident}")) %>% 
  head(15) %>%  select(incident) %>% rename("Theft/Larceny Offense" = incident)

top_burglary <- appended_crime_logs %>% 
  filter(robbery_burglary == 1) %>% 
  count(incident, sort = T) %>% 
  mutate(incident = glue("({n}) {incident}")) %>% 
  head(30) %>%  select(incident) %>% rename("Robbery/Burglary Offense" = incident)

top_categories <- bind_cols(top_sexual_assault, top_alcohol_offense, top_burglary)

