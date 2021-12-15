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

offenses <- list("alcohol_offense_per25", "drug_offense_per25", "sexual_assault_per25")

explanatory_vars <- c("treatment:reason_sexual_assault",
                      "treatment:reason_death",
                      "treatment:reason_behavior",
                      "treatment:reason_unknown")

fe <- c( "day_of_week", "university_by_academic_year", "holiday", "spring_semester")
offenses_regs <- map(offenses, ~ifc::reghdfe(daily_crime_het,  ., explanatory_vars ,fe , "university"))

offenses_regs_weekends <- map(offenses, ~ifc::reghdfe(daily_crime_het_weekends,  ., explanatory_vars
                                                      , fe, "university"))

offenses_regs_weekdays <- map(offenses, ~ifc::reghdfe(daily_crime_het_weekdays,  ., explanatory_vars
                                                      , fe, "university"))


trigger_regs <- c(offenses_regs, offenses_regs_weekends, offenses_regs_weekdays)

trigger_regs <- map_df(trigger_regs, ~broom::tidy(., conf.int = T))

type <- c(rep("Alcohol Offense", 4), rep("Drug Offense", 4), rep("Sexual Assault", 4), rep("Alcohol Offense", 4),
  rep("Drug Offense", 4), rep("Sexual Assault", 4), rep("Alcohol Offense", 4),
  rep("Drug Offense", 4), rep("Sexual Assault", 4))

week_type <- c(rep("All Days", 12), rep("Weekends\n(Fri-Sun)", 12),rep("Weekdays\n(Mon-Thurs)", 12))

trigger_reg_graph <- tibble(trigger_regs, type, week_type) %>% 
  mutate(model = case_when(
    str_detect(term, "sexual_assault") ~ "Trigger: Sexual Assault",
    str_detect(term, "death") ~"Trigger: Fraternity-related Death",
    str_detect(term, "unknown") ~"Trigger: Unspecified",
    str_detect(term, "behavior") ~"Trigger: Behavior")) %>% 
  mutate(estimate = round(estimate, 3)) %>% 
  ggplot(aes(type, estimate, group = week_type, color = week_type)) +
  geom_point(aes(shape = type), position = position_dodge(width = 0.3),
             show.legend = F) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red", alpha = 0.8) +
  facet_wrap(~model) +
  labs(type = " ", group = " ", color = " ", x = " ", y = "Coefficient Estimates and 95% Confidence Intervals") +
  scale_color_manual(values=c("#000000", "#0072B2")) +
  theme_minimal() +
  theme(legend.position = "bottom")
  
trigger_reg_graph <- tibble(trigger_regs, type, week_type) %>% 
  mutate(model = case_when(
    str_detect(term, "sexual_assault") ~ "Trigger: Sexual Assault",
    str_detect(term, "death") ~"Trigger: Fraternity-related Death",
    str_detect(term, "unknown") ~"Trigger: Unspecified",
    str_detect(term, "behavior") ~"Trigger: Behavior")) %>% 
  mutate(estimate = round(estimate, 3)) %>% 
  mutate(week_type = factor(week_type, levels = c("All Days", "Weekends\n(Fri-Sun)", "Weekdays\n(Mon-Thurs)") )) %>% 
  ggplot(aes(week_type, estimate)) +
  geom_point(aes(shape = type)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, linetype = type)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "dark red", alpha = 0.8) +
  facet_grid(type~model, scales = "free") +
  theme_minimal() +
  labs(x = " ", y = "Coefficient Estimates and 95% Confidence Intervals", linetype = " ", shape = " ")+
  theme(legend.position = "bottom", strip.background.x = element_rect(fill = "grey"))



gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-by-Year", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: year", "FE: Year", ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
              "FE: date", "FE: Day-by-Month-by-Year", ~fmt,
              "FE: university_by_year_by_semester_number", "FE: University-by-Year-by-Semester-Number", ~fmt)

trigger_table <- modelsummary(offenses_regs, stars = T,
             gof_omit = 'DF|Deviance|AIC|BIC|Log|R2 Within|R2 Ps|R2|R2 Adj.',
             coef_rename = c("treatment:reason_sexual_assault" = "Moratorium x Triggering Sexual Assault",
                             "treatment:reason_death" = "Moratorium x Triggering Death of Student",
                             "treatment:reason_behavior" = "Motatorium x Triggering Behavior Violation",
                             "treatment:reason_unknown" = "Moratorium x Triggering Event Unknown",
                             "lead_2" = "2 Weeks Before",
                             "lead_1" = "1 Week Before",
                             "lag_1" = "1 Week After",
                             "lag_2" = "2 Weeks After"),
             gof_map = gm,
             notes = list("Standard errors clustered by  university.",
                          "Offenses are per-25000 enrolled students.",
                          "Columns represent the dependent variable.",
                          "Weekends look similar except sexual assault - effects driven by weekdays."),
             title = "\\label{trigger_table}Effect of Moratoriums on Offenses by Triggering Event") %>% 
  add_header_above(c(" " = 1, "Dependent Variable" = 3))
