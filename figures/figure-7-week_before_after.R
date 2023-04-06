library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)
library(patchwork)
library(grid)
library(gridExtra)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv")
}


fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")
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

weeksplit_controls <- bind_rows(alc_weeksplit_controls, sex_weeksplit_controls)


  
  
alc_weeksplit_g <- weeksplit_controls %>%
  mutate(time_grid = case_when(
    time == "Week Before" ~-1,
    time == "In\nMoratorium" ~0,
    time == "Week After" ~ 1
  )) %>% 
  filter(offense == "Alcohol Offense") %>% 
  mutate(in_moratorium = ifelse(time == "In\nMoratorium", 1, 0)) %>% 
  mutate(time = factor(time, levels = c("Week Before", "In\nMoratorium", "Week After"), labels = c("-1 Week", "In\nMoratorium","+1 Week"))) %>% 
  mutate(week_type = factor(week_type, levels = c("All Days", "Weekends", "Weekdays"))) %>% 
  ggplot(aes(estimate, time)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.4) +
  facet_wrap(~week_type) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 2- 0.5,
                ymax = 2+ 0.5),fill = "grey", alpha = 0.1) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_minimal() +
  labs(x = "", y = " ", title = "Panel A: Alcohol Offenses") +
  theme(legend.position = "none",
        strip.text = element_text(size = 7),
        plot.title = element_text(size = 10),
        axis.text.x = element_text(size = 7)) +
  coord_flip() 

sex_weeksplit_g <- weeksplit_controls %>%
  mutate(time_grid = case_when(
    time == "Week Before" ~-1,
    time == "In\nMoratorium" ~0,
    time == "Week After" ~ 1
  )) %>% 
  filter(offense == "Sexual Assault") %>% 
  mutate(in_moratorium = ifelse(time == "In\nMoratorium", 1, 0)) %>% 
  mutate(time = factor(time, levels = c("Week Before", "In\nMoratorium", "Week After"), labels = c("-1 Week", "In\nMoratorium","+1 Week"))) %>% 
  mutate(week_type = factor(week_type, levels = c("All Days", "Weekends", "Weekdays"))) %>% 
  ggplot(aes(estimate, time)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.4) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 2- 0.5,
                ymax = 2+ 0.5),fill = "grey", alpha = 0.1) +
  facet_wrap(~week_type) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
  theme_minimal() +
  labs(x = "", y = " ", title = "Panel B: Sexual Assaults") +
  theme(legend.position = "none",
        strip.text = element_text(size = 7),
        plot.title = element_text(size = 10),
        axis.text.x = element_text(size = 7)) +
  coord_flip() 

result <- alc_weeksplit_g + sex_weeksplit_g + plot_layout(ncol = 1)

week_before_after_graph <- patchwork::patchworkGrob(result)

figure_7 <- gridExtra::grid.arrange(week_before_after_graph, left = textGrob("Coefficient Estimate and 95% Confidence Interval",
                                                                 rot = 90, gp = gpar(fontsize = 10)), bottom = textGrob(""))


ggsave(filename = "figures/michael-topper-figure-7.pdf", plot = figure_7,
       width = 6, height = 4.5)
