library(tidyverse)
library(fixest)
library(modelsummary)
library(ifc)
library(lubridate)

weekly_crime <- read_csv("Created Data/xMaster_data_2021/weekly_panel.csv")
weekly_crime_weekends <- read_csv("Created Data/xMaster_data_2021/weekly_panel_weekends.csv")

es <- ifc::event_study_week(weekly_crime, 8, reference_week = 2)

clean_es <- function(data) {
  cleaned_es <- data %>% 
    mutate(across(c(sexual_assault, alcohol_offense,
                    theft, robbery_burglary, drug_offense), list(ihs = ifc::ihs_transform),
                  .names = "{.fn}_{.col}")) 
  return(cleaned_es)
}

es <- clean_es(es)

x <- es %>% 
  # filter(!(str_detect(university, "_2$"))) %>% 
  # filter(university != "Miami University-Oxford") %>% 
  feols(c(alcohol_offense_per100) ~ treatment_minus_5 +
          treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
          treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 +  ftime_total_undergrad + total_undergrad_asian +
          total_undergrad_hispanic + total_undergrad_black| uni_month + year, cluster = ~university, data = .) %>% summary()
x %>% 
  coefplot()

alcohol_es <- es %>% 
  group_by(university, year) %>% 
  mutate(uni_year = cur_group_id()) %>% 
  ungroup() %>% 
  feols(alcohol_offense_per100 ~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
          treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
          treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
          treatment_plus_7 + treatment_plus_8 | uni_month +  uni_year , cluster = ~university, data = .) 
alcohol_es %>% 
  coefplot()

other_es <- es %>% 
  feols(theft_per100 ~ treatment_minus_8 + treatment_minus_7 + treatment_minus_6 + treatment_minus_5 + 
          treatment_minus_4 + treatment_minus_3 + treatment_minus_2 + treatment_minus_1 + treatment + 
          treatment_plus_1 + treatment_plus_2 + treatment_plus_3 + treatment_plus_4 + treatment_plus_5 + treatment_plus_6 +
          treatment_plus_7 + treatment_plus_8| uni_month + year, cluster = ~university, data = .) 
other_es %>% 
  coefplot
es %>% 
  group_by(university) %>% 
  count(treatment_minus_2) %>% View()

es %>% 
  colnames()

es %>% 
  group_by(university) %>% 
  datasummary_skim()

es %>% 
  filter(alcohol_offense >20) %>% View()
