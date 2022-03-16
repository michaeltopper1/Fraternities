## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-03-10
##

library(tidyverse)
library(lubridate)
library(kableExtra)
library(modelsummary)
library(fixest)
library(tidytext)
library(patchwork)
library(grid)
library(gridExtra)

if (!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}


# adding in indicators for 2 and 1 weeks before and after -----------------

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
    xx <- xx + dplyr::lag(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}

daily_crime <- daily_crime %>% 
  mutate(away_game = ifelse(
    home_game == 0 & game_occurred == 1, 1, 0
  )) 

daily_crime <- daily_crime %>% 
  group_by(university) %>% 
  arrange(date) %>%
  mutate(game_day_weekend = ifelse(game_occurred == 1 & day_of_week %in% c("Fri", "Sat", "Sun"), 1, 0)) %>% 
  mutate(pre_game_day = leead(game_day_weekend, c(1:2))) %>% 
  mutate(post_game_day = laag(game_day_weekend, c(1:2))) %>% 
  mutate(game_weekend = ifelse(game_occurred ==1 | pre_game_day ==1 | post_game_day == 1, 1, 0)) %>% 
  mutate(game_weekend = ifelse(game_weekend ==1 & day_of_week %in% c("Fri", "Sat", "Sun"), 1, 0)) %>% 
  ungroup() 

daily_crime <- daily_crime %>% 
  group_by(university) %>% 
  arrange(date) %>%
  mutate(home_game_day_weekend = ifelse(game_occurred == 1 & home_game == 1 & day_of_week %in% c("Fri", "Sat", "Sun"), 1, 0)) %>% 
  mutate(home_pre_game_day = leead(home_game_day_weekend, c(1:2))) %>% 
  mutate(home_post_game_day = laag(home_game_day_weekend, c(1:2))) %>% 
  mutate(home_game_weekend = ifelse(home_game ==1 | home_pre_game_day ==1 | home_post_game_day == 1, 1, 0)) %>% 
  mutate(home_game_weekend = ifelse(home_game_weekend ==1 & day_of_week %in% c("Fri", "Sat", "Sun"), 1, 0)) %>% 
  ungroup() 

daily_crime <- daily_crime %>% 
  group_by(university) %>% 
  arrange(date) %>%
  mutate(away_game_day_weekend = ifelse(game_occurred == 1 & away_game == 1 & day_of_week %in% c("Fri", "Sat", "Sun"), 1, 0)) %>% 
  mutate(away_pre_game_day = leead(away_game_day_weekend, c(1:2))) %>% 
  mutate(away_post_game_day = laag(away_game_day_weekend, c(1:2))) %>% 
  mutate(away_game_weekend = ifelse(away_game ==1 | away_pre_game_day ==1 | away_post_game_day == 1, 1, 0)) %>% 
  mutate(away_game_weekend = ifelse(away_game_weekend ==1 & day_of_week %in% c("Fri", "Sat", "Sun"), 1, 0)) %>% 
  ungroup() 




fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester")


# effect of game days -----------------------------------------------------


alc_pooled <- ifc::reghdfe(daily_crime, "alcohol_offense_per25", c("game_weekend"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "All Games")
alc_home <- ifc::reghdfe(daily_crime, "alcohol_offense_per25", c("home_game_weekend"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Home Game")
alc_away <- ifc::reghdfe(daily_crime, "alcohol_offense_per25", c("away_game_weekend"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Away Game")


# interaction effects -----------------------------------------------------


alc_pooled_m <- ifc::reghdfe(daily_crime, "alcohol_offense_per25", c("game_weekend:treatment"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "All Games\nx\nIn Moratorium")
alc_home_m <- ifc::reghdfe(daily_crime, "alcohol_offense_per25", c("home_game_weekend:treatment"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Home Game\nx\nIn Moratorium")
alc_away_m <- ifc::reghdfe(daily_crime, "alcohol_offense_per25", c("away_game_weekend:treatment"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Away Game\nx\nIn Moratorium")

alc <- bind_rows(alc_pooled, alc_home, alc_away,
                 alc_pooled_m, alc_home_m, alc_away_m)
alc_game <- alc %>% 
  mutate(row_number = c(1:3, 1:3)) %>% 
  mutate(row_placement = row_number()) %>% 
  mutate(moratorium = ifelse(row_placement > 3, "Effect of Game Day Weekends + In Moratorium", "Effect of Game Day Weekends")) %>% 
  mutate(moratorium = factor(moratorium, levels = c("Effect of Game Day Weekends", "Effect of Game Day Weekends + In Moratorium"))) %>% 
  mutate(type = factor(type, levels = c("All Games", "Home Game","Away Game", "All Games\nx\nIn Moratorium",
                                        "Home Game\nx\nIn Moratorium", "Away Game\nx\nIn Moratorium"))) %>% 
  ggplot(aes(row_number, estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax= conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~moratorium) +
  scale_x_continuous(breaks = c(1:6), labels = c("All Game\nWeekends", "Home Game\nWeekends","Away Game\nWeekends", "All Games\nx\nIn Moratorium",
                                                 "Home Game\nx\nIn Moratorium", "Away Game\nx\nIn Moratorium")) +
  labs(x = " ", y = "", title = "Panel A: Alcohol Offenses") +
  theme_minimal() +
  theme(plot.title = element_text(size=10))




sex_pooled <- ifc::reghdfe(daily_crime, "sexual_assault_per25", c("game_weekend"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "All Games")
sex_home <- ifc::reghdfe(daily_crime, "sexual_assault_per25", c("home_game_weekend"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Home Game")
sex_away <- ifc::reghdfe(daily_crime, "sexual_assault_per25", c("away_game_weekend"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Away Game")


# interaction effects -----------------------------------------------------


sex_pooled_m <- ifc::reghdfe(daily_crime, "sexual_assault_per25", c("game_weekend:treatment"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "All Games\nx\nIn Moratorium")
sex_home_m <- ifc::reghdfe(daily_crime, "sexual_assault_per25", c("home_game_weekend:treatment"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Home Game\nx\nIn Moratorium")
sex_away_m <- ifc::reghdfe(daily_crime, "sexual_assault_per25", c("away_game_weekend:treatment"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Away Game\nx\nIn Moratorium")

sex <- bind_rows(sex_pooled, sex_home, sex_away,
                 sex_pooled_m, sex_home_m, sex_away_m)

sex_game <- sex %>% 
  mutate(row_number = c(1:3, 1:3)) %>% 
  mutate(row_placement = row_number()) %>% 
  mutate(moratorium = ifelse(row_placement > 3, "Effect of Game Day Weekends + In Moratorium", "Effect of Game Day Weekends")) %>% 
  mutate(moratorium = factor(moratorium, levels = c("Effect of Game Day Weekends", "Effect of Game Day Weekends + In Moratorium"))) %>% 
  mutate(type = factor(type, levels = c("All Games", "Home Game","Away Game", "All Games\nx\nIn Moratorium",
                                        "Home Game\nx\nIn Moratorium", "Away Game\nx\nIn Moratorium"))) %>% 
  ggplot(aes(row_number, estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax= conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~moratorium) +
  scale_x_continuous(breaks = c(1:6), labels = c("All Game\n Weekends", "Home Game\n Weekends","Away Game\nWeekends", "All Games\nx\nIn Moratorium",
                                                 "Home Game\nx\nIn Moratorium", "Away Game\nx\nIn Moratorium")) +
  labs(x = " ", y = "", title = "Panel B: Sexual Assaults") +
  theme_minimal() +
  theme(plot.title = element_text(size=10))

game_weekend_plot <- alc_game + sex_game + plot_layout(nrow = 2)
