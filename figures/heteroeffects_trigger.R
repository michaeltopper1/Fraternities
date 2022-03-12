## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-07-23
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(lubridate)
library(kableExtra)
library(ggrepel)
library(patchwork)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv") 
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}
if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv")
}


daily_crime_het <- daily_crime %>% 
  mutate(across(c(reason1, reason2), ~ifelse(is.na(.), "untreated", .))) %>% 
  mutate(reason_sexual_assault = ifelse((reason1 == 'sexual assault' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
                                        | (reason2 == 'sexual assault' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>% 
  mutate(reason_death = ifelse((reason1 == 'death' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
                               | (reason2 == 'death' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>% 
  mutate(reason_behavior = ifelse((reason1 == 'behavior' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
                                  | (reason2 == 'behavior' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>% 
  mutate(reason_unknown = ifelse((reason1 == 'unknown' & (date >= closure_1 & date < closure_1_end)  & treatment == 1) 
                                 | (reason2 == 'unknown' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0))

daily_crime_het_weekends <- daily_crime_weekends %>%
  mutate(across(c(reason1, reason2), ~ifelse(is.na(.), "untreated", .))) %>%
  mutate(reason_sexual_assault = ifelse((reason1 == 'sexual assault' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                                        | (reason2 == 'sexual assault' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>%
  mutate(reason_death = ifelse((reason1 == 'death' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                               | (reason2 == 'death' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>%
  mutate(reason_behavior = ifelse((reason1 == 'behavior' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                                  | (reason2 == 'behavior' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>%
  mutate(reason_unknown = ifelse((reason1 == 'unknown' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                                 | (reason2 == 'unknown' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0))
daily_crime_het_weekdays <- daily_crime_weekdays %>% 
  mutate(across(c(reason1, reason2), ~ifelse(is.na(.), "untreated", .))) %>%
  mutate(reason_sexual_assault = ifelse((reason1 == 'sexual assault' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                                        | (reason2 == 'sexual assault' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>%
  mutate(reason_death = ifelse((reason1 == 'death' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                               | (reason2 == 'death' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>%
  mutate(reason_behavior = ifelse((reason1 == 'behavior' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                                  | (reason2 == 'behavior' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0)) %>%
  mutate(reason_unknown = ifelse((reason1 == 'unknown' & (date >= closure_1 & date < closure_1_end)  & treatment == 1)
                                 | (reason2 == 'unknown' & (date >= closure_2 & date < closure_2_end) & treatment == 1) , 1, 0))

offenses <- list("alcohol_offense_per25", "sexual_assault_per25")

explanatory_vars <- c("treatment:reason_sexual_assault",
                      "treatment:reason_death",
                      "treatment:reason_behavior",
                      "treatment:reason_unknown")

fe <- c( "day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")
offenses_regs <- map(offenses, ~ifc::reghdfe(daily_crime_het,  ., explanatory_vars ,fe , "university"))

offenses_regs_weekends <- map(offenses, ~ifc::reghdfe(daily_crime_het_weekends,  ., explanatory_vars
                                                      , fe, "university"))

offenses_regs_weekdays <- map(offenses, ~ifc::reghdfe(daily_crime_het_weekdays,  ., explanatory_vars
                                                      , fe, "university"))


trigger_regs <- c(offenses_regs, offenses_regs_weekends, offenses_regs_weekdays)

trigger_regs <- map_df(trigger_regs, ~broom::tidy(., conf.int = T))

type <- c(rep("Alcohol Offense", 4), rep("Sexual Assault", 4), rep("Alcohol Offense", 4),rep("Sexual Assault", 4), rep("Alcohol Offense", 4),
 rep("Sexual Assault", 4))

week_type <- c(rep("All Days", 8), rep("Weekends\n(Fri-Sun)", 8),rep("Weekdays\n(Mon-Thurs)", 8))

  

  
alc_trigger_reg <- tibble(trigger_regs, type, week_type) %>% 
  mutate(model = case_when(
    str_detect(term, "sexual_assault") ~ "Trigger: Sexual Assault",
    str_detect(term, "death") ~"Trigger: Fraternity-related Death",
    str_detect(term, "unknown") ~"Trigger: Unspecified",
    str_detect(term, "behavior") ~"Trigger: Behavior")) %>% 
  mutate(estimate = round(estimate, 3)) %>% 
  mutate(week_type = factor(week_type, levels = c("All Days", "Weekends\n(Fri-Sun)", "Weekdays\n(Mon-Thurs)") )) %>% 
  filter(type == "Alcohol Offense") %>% 
  ggplot(aes(week_type, estimate)) +
  geom_point(aes(shape = type)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "dark red", alpha = 0.8) +
  facet_wrap(~model, ncol = 4) +
  theme_minimal() +
  labs(x = " ", y = "", linetype = " ", shape = " ", title = "Panel A: Alcohol Offenses") +
  theme(legend.position = "none", strip.background.x = element_rect(fill = "grey"),
        plot.title = element_text(size=10))

sex_trigger_reg <- tibble(trigger_regs, type, week_type) %>% 
  mutate(model = case_when(
    str_detect(term, "sexual_assault") ~ "Trigger: Sexual Assault",
    str_detect(term, "death") ~"Trigger: Fraternity-related Death",
    str_detect(term, "unknown") ~"Trigger: Unspecified",
    str_detect(term, "behavior") ~"Trigger: Behavior")) %>% 
  mutate(estimate = round(estimate, 3)) %>% 
  mutate(week_type = factor(week_type, levels = c("All Days", "Weekends\n(Fri-Sun)", "Weekdays\n(Mon-Thurs)") )) %>% 
  filter(type == "Sexual Assault") %>% 
  ggplot(aes(week_type, estimate)) +
  geom_point(aes(shape = type)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "dark red", alpha = 0.8) +
  facet_wrap(~model, ncol = 4) +
  theme_minimal() +
  labs(x = " ", y = "", linetype = " ", shape = " ", title = "Panel B: Sexual Assaults") +
  theme(legend.position = "none", strip.background.x = element_rect(fill = "grey"),
        plot.title = element_text(size=10)) 

result <- alc_trigger_reg + sex_trigger_reg + plot_layout(ncol = 1)

trigger_reg_graph <- patchwork::patchworkGrob(result)