library(tidyverse)
library(modelsummary)
library(fixest)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}


# defining lead and lag functions that can take vectors -------------------

leead <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + lead(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}
laag <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + lag(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}


## getting the beginning of the treatment so I know where to put the leads and lags
daily_crime_within <- daily_crime %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(closure_ind = ifelse(closure_1 == date | (closure_2 == date),1 ,0),
         closure_end_ind = ifelse(closure_1_end == date | closure_2_end == date, 1, 0)) %>% 
  mutate(closure_ind = ifelse(university == "Florida International University" & date == "2018-01-04", 1, closure_ind)) %>% 
  mutate(across(c(closure_ind, closure_end_ind), ~ifelse(is.na(.), 0, .)))


## getting mutually exclusive bins of the treatment variable
daily_crime_within <- daily_crime_within %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(within_1 = laag(closure_ind, c(0:14)),
         within_2 = laag(closure_ind, c(15:28)),
         within_3 = laag(closure_ind, c(29:42)),
         within_4 = laag(closure_ind, c(43:56)),
         within_two_months = laag(closure_ind, c(57:70)),
         within_three_months = laag(closure_ind, c(71, 84)),
         within_over_three_months = laag(closure_ind, c(85:500))
         ) %>% 
  mutate(across(starts_with("within"), ~ifelse(treatment == 0 & . == 1, 0, .))) %>% 
  ungroup()
  # filter(day_of_week == "Mon" | day_of_week == "Tue" | day_of_week == "Wed" | day_of_week == "Thu") %>%
# filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun")
  


explanatory_vars <- c("within_1",  "within_2", "within_3", "within_4",
                      "within_two_months", "within_three_months", "within_over_three_months")
fe <- c("university", "date")
ifc::reghdfe(daily_crime_within , "alcohol_offense_per25", explanatory_vars, fe, "university") %>% summary()
number_identified_by <- c()
for (i in 1:length(explanatory_vars)){
  number_identified_by[i] <- daily_crime_within %>% 
    filter(!!sym(explanatory_vars[i]) == 1) %>% 
    distinct(university) %>% 
    nrow()
}

## creating timeline of the mutually exclusive treatments
timeline <- c("Weeks 1 & 2", "Weeks 3 & 4", "Weeks 5 & 6", "Weeks 7 & 8", "Weeks 9 & 10", "Weeks 11 & 12", "Weeks 13+")
timing <- tibble(number_identified_by, timeline, time = c(1:7))  %>% 
  mutate(timeline = glue::glue("{timeline} 
                               ({number_identified_by})"))

## pulling the timeline for ggplot labeling
timeline <- timing %>% 
  pull(-2)



# regressions -------------------------------------------------------------


alc_progression <- ifc::reghdfe(daily_crime_within, "alcohol_offense_per25", explanatory_vars, fe, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  bind_cols(timing) %>% 
  mutate(day_type = "Full Sample")
alc_progression_weekend <- ifc::reghdfe(daily_crime_within %>% 
               filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun") , "alcohol_offense_per25", explanatory_vars, fe, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  bind_cols(timing) %>% 
  mutate(day_type = "Weekends (Fri-Sun)")


drug_progression <- ifc::reghdfe(daily_crime_within, "drug_offense_per25", explanatory_vars, fe, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  bind_cols(timing) %>% 
  mutate(day_type = "Full Sample")

drug_progression_weekend <- ifc::reghdfe(daily_crime_within %>% 
                                           filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun") ,
                                         "drug_offense_per25", explanatory_vars, fe, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  bind_cols(timing) %>% 
  mutate(day_type = "Weekends (Fri-Sun)")


sex_progression <- ifc::reghdfe(daily_crime_within, "sexual_assault_per25", explanatory_vars, fe, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  bind_cols(timing) %>% 
  mutate(day_type = "Full Sample")

sex_progression_weekend <- ifc::reghdfe(daily_crime_within %>% 
                                          filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun") ,
                                        "sexual_assault_per25", explanatory_vars, fe, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  bind_cols(timing) %>% 
  mutate(day_type = "Weekends (Fri-Sun)")



# figures -----------------------------------------------------------------


cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

mprogression_alc <- alc_progression %>% 
  bind_rows(alc_progression_weekend) %>% 
  ggplot(aes(x = time, y = estimate, group = day_type)) +
  geom_path(aes(linetype = day_type)) +
  geom_point(aes(shape = day_type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = day_type), alpha =0.2) +
  scale_x_continuous(breaks = c(1:7), labels = timeline) +
  geom_hline(yintercept = 0, color = "dark red")  +
  labs(x = "", y = "Coefficient Estimate", group = " ", linetype = " ", shape = " ", fill = " ", color = " ") +
  scale_fill_manual(values=cbbPalette) +
  scale_color_manual(values = cbbPalette) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~day_type)


mprogression_drug <- drug_progression %>% 
  bind_rows(drug_progression_weekend) %>% 
  ggplot(aes(x = time, y = estimate, group = day_type)) +
  geom_path(aes(linetype = day_type)) +
  geom_point(aes(shape = day_type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = day_type), alpha =0.2) +
  scale_x_continuous(breaks = c(1:7), labels = timeline) +
  geom_hline(yintercept = 0, color = "dark red")  +
  labs(x = "", y = "Coefficient Estimate", group = " ", linetype = " ", shape = " ", fill = " ", color = " ") +
  scale_fill_manual(values=cbbPalette) +
  scale_color_manual(values = cbbPalette) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~day_type)


mprogression_sex <- sex_progression %>% 
  bind_rows(sex_progression_weekend) %>% 
  ggplot(aes(x = time, y = estimate, group = day_type)) +
  geom_path(aes(linetype = day_type)) +
  geom_point(aes(shape = day_type)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = day_type), alpha =0.2) +
  scale_x_continuous(breaks = c(1:7), labels = timeline) +
  geom_hline(yintercept = 0, color = "dark red")  +
  labs(x = "", y = "Coefficient Estimate", group = " ", linetype = " ", shape = " ", fill = " ", color = " ") +
  scale_fill_manual(values=cbbPalette) +
  scale_color_manual(values = cbbPalette) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~day_type)

