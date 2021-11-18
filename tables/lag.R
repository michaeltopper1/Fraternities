library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv")
}


daily_crime <- daily_crime %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(lead_3 = lead(lead_2, 7)) %>% 
  mutate(lead_4 = lead(lead_3, 7)) %>% 
  mutate(lead_5 = lead(lead_4, 7)) %>% 
  mutate(lead_6 = lead(lead_5, 7)) %>% 
  mutate(lead_7 = lead(lead_6, 7)) %>% 
  mutate(lead_8 = lead(lead_7, 7)) %>% 
  mutate(lag_3 = lag(lag_2, 7)) %>% 
  mutate(lag_4 = lag(lag_3, 7)) %>% 
  mutate(lag_5 = lag(lag_4, 7)) %>% 
  mutate(lag_6 = lag(lag_5, 7)) %>% 
  mutate(lag_7 = lag(lag_6, 7)) %>% 
  mutate(lag_8 = lag(lag_7, 7)) %>% 
  ungroup()

daily_crime %>% 
  group_by(university) %>% 
  mutate(moratorium_1_start = ifelse(closure_1 == date, 1, 0)) %>% 
  mutate(moratorium_1_end = ifelse(closure_1_end == date, 1, 0)) %>% 
  mutate(moratorium_2_start = ifelse(closure_2 == date, 1, 0)) %>% 
  mutate(moratorium_2_end = ifelse(closure_2_end == date, 1, 0)) %>% 
  mutate(leadx_1 = lead(moratorium_1_start, c(1:7))) %>% 
  select(leadx_1, treatment, university, moratorium_1_start,date) %>% 
  filter(university == "San Diego State University") %>% View()
  mutate(across(starts_with('lead_'), ~ifelse(. == 1 & treatment == 1)))
explanatory <- c("lead_8", "lead_7","lead_6", "lead_5", "lead_4", "lead_3", "lead_2", "lead_1", "treatment",
                 "lag_1", "lag_2", "lag_3", "lag_4", "lag_5", "lag_6", "lag_7", "lag_8")

model <- ifc::reghdfe(daily_crime , "alcohol_offense_per25", explanatory, fixed_effects = fixed_effects_preferred, cluster = "university")

model %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(weeks = -8:8) %>% 
  ggplot(aes(weeks, estimate)) +
  geom_point() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high),fill = "light grey", alpha =0.8) +
  geom_path(linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, color = "dark red") +
  theme_minimal()
car::linearHypothesis(model, c("lead_8 = 0", "lead_7 = 0","lead_6 = 0",
                               "lead_5 = 0",
                               "lead_4 = 0",
                               "lead_3 = 0", 
                               "lead_2 = 0",
                               "lead_1 = 0"))
