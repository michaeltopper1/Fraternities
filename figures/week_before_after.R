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


fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester")
explanatory_vars_week_before <- c("week_before", "treatment", "week_after")

data_subsets <- list(daily_crime, daily_crime_weekends, daily_crime_weekdays)



alc_weeksplit_controls <- map_df(data_subsets, ~ifc::reghdfe(., c("alcohol_offense_per25"),explanatory_vars_week_before, fixed_effects_preferred, "university")
                                 %>% 
                                   broom::tidy(conf.int  = T)) %>% 
  mutate(week_type = c(rep("All Days", 3), rep("Weekends", 3), rep("Weekdays", 3)),
         offense = "Alcohol Offense",
         time = c(rep(c("Week Before", "In\nMoratorium", "Week After"), 3)))
drug_weeksplit_controls <- map_df(data_subsets, ~ifc::reghdfe(., c("drug_offense_per25"),explanatory_vars_week_before, fixed_effects_preferred, "university") %>% 
                                 broom::tidy(conf.int  = T) 
) %>% 
  mutate(week_type = c(rep("All Days", 3), rep("Weekends", 3), rep("Weekdays", 3)),
         offense = "Drug Offense",
         time = c(rep(c("Week Before", "In\nMoratorium", "Week After"), 3)))

sex_weeksplit_controls <- map_df(data_subsets, ~ifc::reghdfe(., c("sexual_assault_per25"),explanatory_vars_week_before, fixed_effects_preferred, "university") 
%>% broom::tidy(conf.int  = T)) %>%
  mutate(week_type = c(rep("All Days", 3), rep("Weekends", 3), rep("Weekdays", 3)),
         offense = "Sexual Assault",
         time = c(rep(c("Week Before", "In\nMoratorium", "Week After"), 3)))

weeksplit_controls <- bind_rows(alc_weeksplit_controls, drug_weeksplit_controls, sex_weeksplit_controls)

week_before_after_graph <- weeksplit_controls %>%
  mutate(time_grid = case_when(
    time == "Week Before" ~-1,
    time == "In\nMoratorium" ~0,
    time == "Week After" ~ 1
  )) %>% 
  mutate(in_moratorium = ifelse(time == "In\nMoratorium", 1, 0)) %>% 
  mutate(time = factor(time, levels = c("Week Before", "In\nMoratorium", "Week After"), labels = c("-1 Week", "In\nMoratorium","+1 Week"))) %>% 
  mutate(week_type = factor(week_type, levels = c("All Days", "Weekends", "Weekdays"))) %>% 
  ggplot(aes(estimate, time, color = in_moratorium)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high)) +
  facet_wrap(~offense + week_type, scales = "free") +
  geom_vline(xintercept = 0, color = "dark red", linetype = "dashed") +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Coefficient Estimate and 95% Confidence Interval", y = " ") +
  coord_flip() 
