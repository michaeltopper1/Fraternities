
library(tidyverse)
library(lubridate)
library(fixest)
lead_endpoint <- 8
lag_endpoint <- 9
leads <- c(1:lead_endpoint)  *7
lags <- c(1:lag_endpoint) * 7

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv") %>% 
  filter(university %in% ifc::moratorium_schools())

daily_crime_es <- daily_crime %>% 
  mutate(closure_1_floor = lubridate::floor_date(closure_1, unit = "day")) %>%
  mutate(closure_2_floor = floor_date(closure_2, unit = "day")) %>% 
  relocate(date, closure_1, closure_1_floor) %>% 
  mutate(beta_0 = ifelse(closure_1_floor == date | closure_2_floor == date, 1, 0)) %>% 
  mutate(beta_0 = ifelse(is.na(beta_0), 0, beta_0)) %>% 
  relocate(beta_0) 

## up to here correct - i have betas that flip on and off

for (i in leads) {
  column_name <- paste0("beta_lead_",i/7)
  daily_crime_es <- daily_crime_es %>% 
    mutate(!!sym(column_name) := ifelse(dplyr::lead(beta_0, n = i) == 1,1,0 ))
}


for (i in lags) {
  column_name <- paste0("beta_lag_",i/7)
  daily_crime_es <- daily_crime_es %>% 
    mutate(!!sym(column_name) := ifelse(dplyr::lag(beta_0, n = i) == 1,1,0 ))
}

lead_columns <-  rep(" ", 7)
for (i in 1:length(leads)) {
  column <- paste0("beta_lead_", i)
  lead_columns[i] <- column
}


lag_columns <- rep(" ", 8)
for (i in 1:8) {
  column <- paste0("beta_lag_", i)
  lag_columns[i+1] <- column
}
lag_columns[1] <- "beta_0"

for (i in 1:9) {
  column <- lag_columns[i]
  daily_crime_es <- daily_crime_es %>% 
    mutate(!!sym(column) := ifelse((date >= closure_1_floor + days(lags[i]- 6) & date <= closure_1_floor + days(lags[i])) |
                                      (date >= closure_2_floor + days(lags[i]- 6) & date <= closure_2_floor + days(lags[i])),1 ,0))
}

for (i in 1:8) {
  column <- lead_columns[i]
  daily_crime_es <- daily_crime_es %>% 
    mutate(!!sym(column) := ifelse((date >= closure_1_floor - days(leads[i]) & date < closure_1_floor - days(leads[i] - 7)) |
                                     (date >= closure_2_floor - days(leads[i]) & date < closure_2_floor - days(leads[i] - 7)),1 ,0))
}


## now need to create the binned column which will be the sum of the 1s - create function for this. need to arrange this 

## getting the first lead to be binned
last_lead <- paste0("beta_lead_", lead_endpoint)
daily_crime_es <- daily_crime_es %>% 
  group_by(university) %>% 
  arrange(desc(date)) %>% 
  mutate(beta_lead_binned = ifelse(!!sym(last_lead) ==1, 1, NA)) %>% 
  mutate(beta_lead_binned = ifelse(!!sym(last_lead) > 0, cumsum(!!sym(last_lead)), NA)) %>% 
  relocate(beta_lead_binned, starts_with("beta")) %>% 
  fill(beta_lead_binned, .direction = "down")  %>% ungroup() %>% arrange(date)

## getting the last lag to be binned
last_lag <- paste0("beta_lag_", lag_endpoint-1)
daily_crime_es <- daily_crime_es %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(beta_lag_binned = ifelse(!!sym(last_lag) == 1, 1, NA)) %>% 
  mutate(beta_lag_binned = ifelse(!!sym(last_lag) > 0, cumsum(!!sym(last_lag)), NA)) %>% 
  relocate(beta_lag_binned) %>% 
  fill(beta_lag_binned, .direction = "down")  %>% ungroup()

daily_crime_es <- daily_crime_es %>% 
  mutate(beta_lag_binned = case_when(beta_lag_binned >= 1 & beta_lag_binned <= 7 ~1,
                                     beta_lag_binned > 7  ~ 2,
                                     TRUE ~as.double(0))) %>% 
  mutate(beta_lead_binned = case_when(beta_lead_binned >= 1 & beta_lead_binned <=7 ~ 1,
                                      beta_lead_binned > 7 ~ 2,
                                      TRUE ~as.double(0)))


es_sex <- daily_crime_es %>% 
  feols(drug_offense_per25 ~ beta_lead_binned + beta_lead_7 + beta_lead_6 + beta_lead_5 + beta_lead_4 + beta_lead_3 + beta_lead_2 + beta_0 +
          beta_lag_1 + beta_lag_2 + beta_lag_3 + beta_lag_4 + beta_lag_5 +
          beta_lag_6 + beta_lag_7 + beta_lag_binned| university_by_semester_number, cluster = ~university, data = .) 

event_study_func(es_sex, 8) +
  geom_hline(yintercept = 0, color = "dark red") 

 daily_crime_es %>% 
  select(beta_0, starts_with("beta_lag_"), "treatment", university, date,closure_1_floor, -beta_lag_9) %>% 
  filter(if_any(starts_with("beta"), ~. == 1)) %>% 
  filter(university == "California Polytechnic State University-San Luis Obispo") %>% View()


