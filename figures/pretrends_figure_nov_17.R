library(tidyverse)
library(modelsummary)
library(fixest)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv")
}


explanatory_vars <- c("lead_2","lead_1", "treatment", "lag_1", "lag_2")

# fixed effects for daily_level -------------------------------------------
fixed_effects_1 <- c("day_of_week", "academic_year", "spring_semester", "university")



# list of data to loop through --------------------------------------------
data <- list(daily_crime, daily_crime_weekends, daily_crime_weekdays)



# get axis and facet wraps ------------------------------------------------
type <- c(rep("Full Sample", 5), rep("Weekends", 5), rep("Weekdays", 5))
period <- c(-2:2, -2:2,-2:2)



# estimation --------------------------------------------------------------

alcohol_trends <- map_df(data, ~ifc::reghdfe(., "alcohol_offense_per25",explanatory_vars,fixed_effects_1 , "university") %>% 
         broom::tidy(conf.int = T)) %>% 
  mutate(week = type) %>% 
  mutate(time = period)

drug_trends <- map_df(data, ~ifc::reghdfe(., "drug_offense_per25",explanatory_vars,fixed_effects_1 , "university") %>% 
                        broom::tidy(conf.int = T)) %>% 
  mutate(week = type) %>% 
  mutate(time = period)

sex_trends <- map_df(data, ~ifc::reghdfe(., "sexual_assault_per25",explanatory_vars,fixed_effects_1 , "university") %>% 
                       broom::tidy(conf.int = T)) %>% 
  mutate(week = type) %>% 
  mutate(time = period)


# plot function -----------------------------------------------------------

trend_graph <- function(x) {
  plot <- x %>% ggplot(aes(time, estimate)) +
    geom_line(aes(linetype = week)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "light grey", alpha =0.5) +
    geom_hline(yintercept = 0, color = "dark red") +
    geom_point(aes(shape =week)) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    facet_wrap(~week)
}

# plots -------------------------------------------------------------------

a_trends <- alcohol_trends %>% 
  trend_graph() +
  labs(x = "Weeks to moratorium", y = "Coefficient Estimate and 95 Percent Confidence Interval", linetype = " ", shape = " ") +
  scale_x_continuous(labels = c("-2", "-1", "Moratorium", "1", "2")) 

d_trends <- drug_trends %>% 
  ggplot(aes(time, estimate)) +
  geom_line(aes(linetype = week)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "light grey", alpha =0.5) +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_point(aes(shape =week)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = c("-2", "-1", "Moratorium", "1", "2")) +
  facet_wrap(~week) +
  labs(x = "Weeks to moratorium", y = "Coefficient Estimate and 95 Percent Confidence Interval", linetype = " ", shape = " ")

s_trends <- sex_trends %>% 
  ggplot(aes(time, estimate)) +
  geom_line(aes(linetype = week)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "light grey", alpha =0.5) +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_point(aes(shape =week)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(labels = c("-2", "-1", "Moratorium", "1", "2")) +
  facet_wrap(~week) +
  labs(x = "Weeks to moratorium", y = "Coefficient Estimate and 95 Percent Confidence Interval", linetype = " ", shape = " ")




daily_crime <- daily_crime %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(median_before = lead(treatment, 42),
         median_after = lag(treatment, 42))

daily_crime_weekends_2 <- daily_crime %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(median_before = lead(treatment, 42),
         median_after = lag(treatment, 42)) %>% 
  filter(day_of_week == "Sat" | day_of_week == "Sun" | day_of_week == "Fri")

daily_crime_weekdays_2 <- daily_crime %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(median_before = lead(treatment, 42),
         median_after = lag(treatment, 42)) %>% 
  filter(day_of_week == "Mon" | day_of_week == "Tue" | day_of_week == "Wed" | day_of_week == "Thu")

data_2 <- list(daily_crime, daily_crime_weekends_2, daily_crime_weekdays_2)

explanatory_vars_2 <- c("median_before", "treatment", "median_after")


type_2 <- c(rep("Full Sample", 3), rep("Weekends", 3), rep("Weekdays", 3))
period_2 <- c(-1:1, -1:1,-1:1)


alc_trends_2 <- map_df(data_2, ~ifc::reghdfe(., "alcohol_offense_per25",explanatory_vars_2,fixed_effects_1 , "university") %>% 
         broom::tidy(conf.int = T)) %>% 
  mutate(week = type_2) %>% 
  mutate(time = period_2)

drug_trends_2 <- map_df(data_2, ~ifc::reghdfe(., "drug_offense_per25",explanatory_vars_2,fixed_effects_1 , "university") %>% 
                          broom::tidy(conf.int = T)) %>% 
  mutate(week = type_2) %>% 
  mutate(time = period_2)

sex_trends_2 <- map_df(data_2, ~ifc::reghdfe(., "sexual_assault_per25",explanatory_vars_2,fixed_effects_1 , "university") %>% 
                         broom::tidy(conf.int = T)) %>% 
  mutate(week = type_2) %>% 
  mutate(time = period_2)


a_trends_2 <- alc_trends_2 %>% 
  ggplot(aes(time, estimate)) +
  geom_line(aes(linetype = week)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "light grey", alpha =0.5) +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_point(aes(shape =week)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(-1:1), labels = c("-6", "Moratorium", "+6")) +
  facet_wrap(~week) +
  labs(x = " ", y = "Coefficient Estimate and 95 Percent Confidence Interval", linetype = " ", shape = " ")

d_trends_2 <- drug_trends_2 %>% 
  ggplot(aes(time, estimate)) +
  geom_line(aes(linetype = week)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "light grey", alpha =0.5) +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_point(aes(shape =week)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(-1:1), labels =c("-6", "Moratorium", "+6")) +
  facet_wrap(~week) +
  labs(x = " ", y = "Coefficient Estimate and 95 Percent Confidence Interval", linetype = " ", shape = " ")

s_trends_2 <- sex_trends_2 %>% 
  ggplot(aes(time, estimate)) +
  geom_line(aes(linetype = week)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "light grey", alpha =0.5) +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_point(aes(shape =week)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = c(-1:1), labels = c("-6", "Moratorium in Place", "+6")) +
  facet_wrap(~week) +
  labs(x = " ", y = "Coefficient Estimate and 95 Percent Confidence Interval", linetype = " ", shape = " ")
