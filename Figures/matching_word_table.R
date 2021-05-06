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
source("Code/Cleaning_crime_log_files/append_daily_crime_logs_1.R")

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
noise_words <- noise_violation_identifier %>% 
  str_replace_all("\\|", ", ")
words <- list(sexual_assault_words, alcohol_words, drug_offense_words, robbery_burglary_words, theft_words,
              noise_words)  %>% 
  unlist()
categories <- c("Sexual Assault", "Alcohol Violations", "Drug Offense",
                "Robbery/Burglary", "Theft/Larceny", "Noise Complaints")
matching_table <- tibble("Outcome" = categories, "Words to Match" = words)


top_drug_alcohol <- appended_crime_logs %>% 
  filter(drug_offense == 1 & alcohol_offense == 1) %>% 
  count(incident, sort = T)  %>% 
  head(15) %>% 
  mutate(incident = glue("({n}) {incident}")) %>% 
  select(incident) %>% rename("Drug and Alcohol" = incident)

top_sexual_assault <- appended_crime_logs %>% 
  filter(sexual_assault == 1) %>% 
  count(incident, sort = T) %>% 
  head(15) %>% 
  mutate(incident = glue("({n}) {incident}")) %>% 
  select(incident) %>% rename("Sexual Assault" = incident)

top_alcohol_offense <- appended_crime_logs %>% 
  filter(alcohol_offense == 1) %>% 
  count(incident, sort = T) %>% 
  mutate(incident = glue("({n}) {incident}")) %>% 
  head(15) %>% select(incident) %>% rename("Alcohol Offense" = incident)

top_drug_offense <- appended_crime_logs %>% 
  filter(drug_offense == 1) %>% 
  count(incident, sort = T) %>% 
  mutate(incident = glue("({n}) {incident}")) %>% 
  head(15) %>%  select(incident) %>% rename("Drug Offense" = incident)

top_noise_offense <- appended_crime_logs %>% 
  filter(noise_offense == 1) %>% 
  count(incident, sort = T) %>% 
  mutate(incident = glue("({n}) {incident}")) %>% 
  head(15) %>%  select(incident) %>% rename("Noise Offense" = incident)

top_theft <- appended_crime_logs %>% 
  filter(theft == 1) %>% 
  count(incident, sort = T) %>% 
  mutate(incident = glue("({n}) {incident}")) %>% 
  head(15) %>%  select(incident) %>% rename("Theft/Larceny Offense" = incident)

top_burglary <- appended_crime_logs %>% 
  filter(robbery_burglary == 1) %>% 
  count(incident, sort = T) %>% 
  mutate(incident = glue("({n}) {incident}")) %>% 
  head(15) %>%  select(incident) %>% rename("Burglary Offense" = incident)

top_categories <- bind_cols(top_sexual_assault, top_alcohol_offense, 
                            top_drug_offense, top_theft, top_burglary, top_noise_offense)

