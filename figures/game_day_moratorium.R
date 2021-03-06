## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-03-05
##

library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)
library(patchwork)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv") 
}

daily_crime <- daily_crime %>% 
  mutate(home_game = ifelse(is.na(home_game), 0, home_game)) %>% 
  mutate(away_game = ifelse(
    home_game == 0 & game_occurred == 1, 1, 0
  )) 


fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester")


# effect of game days -----------------------------------------------------


alc_pooled <- ifc::reghdfe(daily_crime, "alcohol_offense_per25", c("game_occurred"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "All Games")
alc_home <- ifc::reghdfe(daily_crime, "alcohol_offense_per25", c("home_game"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Home Game")
alc_away <- ifc::reghdfe(daily_crime, "alcohol_offense_per25", c("away_game"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Away Game")


# interaction effects -----------------------------------------------------


alc_pooled_m <- ifc::reghdfe(daily_crime, "alcohol_offense_per25", c("game_occurred:treatment"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "All Games\nx\nIn Moratorium")
alc_home_m <- ifc::reghdfe(daily_crime, "alcohol_offense_per25", c("home_game:treatment"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Home Game\nx\nIn Moratorium")
alc_away_m <- ifc::reghdfe(daily_crime, "alcohol_offense_per25", c("away_game:treatment"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Away Game\nx\nIn Moratorium")

alc <- bind_rows(alc_pooled, alc_home, alc_away,
                 alc_pooled_m, alc_home_m, alc_away_m)

alc_game <- alc %>% 
  mutate(row_number = c(1:3, 1:3)) %>% 
  mutate(row_placement = row_number()) %>% 
  mutate(moratorium = ifelse(row_placement > 3, "Effect of Game Day + In Moratorium", "Effect of Game Days")) %>% 
  mutate(moratorium = factor(moratorium, levels = c("Effect of Game Days", "Effect of Game Day + In Moratorium"))) %>% 
  mutate(type = factor(type, levels = c("All Games", "Home Game","Away Game", "All Games\nx\nIn Moratorium",
                                        "Home Game\nx\nIn Moratorium", "Away Game\nx\nIn Moratorium"))) %>% 
  ggplot(aes(row_number, estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax= conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~moratorium) +
  scale_x_continuous(breaks = c(1:6), labels = c("All Games", "Home Game","Away Game", "All Games\nx\nIn Moratorium",
                              "Home Game\nx\nIn Moratorium", "Away Game\nx\nIn Moratorium")) +
  labs(x = " ", y = "", title = "Panel A: Alcohol Offenses") +
  theme_minimal() 




# effect of game days -----------------------------------------------------


sex_pooled <- ifc::reghdfe(daily_crime, "sexual_assault_per25", c("game_occurred"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "All Games")
sex_home <- ifc::reghdfe(daily_crime, "sexual_assault_per25", c("home_game"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Home Game")
sex_away <- ifc::reghdfe(daily_crime, "sexual_assault_per25", c("away_game"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Away Game")


# interaction effects -----------------------------------------------------


sex_pooled_m <- ifc::reghdfe(daily_crime, "sexual_assault_per25", c("game_occurred:treatment"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "All Games\nx\nIn Moratorium")
sex_home_m <- ifc::reghdfe(daily_crime, "sexual_assault_per25", c("home_game:treatment"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Home Game\nx\nIn Moratorium")
sex_away_m <- ifc::reghdfe(daily_crime, "sexual_assault_per25", c("away_game:treatment"), fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(type = "Away Game\nx\nIn Moratorium")

sex <- bind_rows(sex_pooled, sex_home, sex_away,
                 sex_pooled_m, sex_home_m, sex_away_m)

sex_game <- sex %>% 
  mutate(row_number = c(1:3, 1:3)) %>% 
  mutate(row_placement = row_number()) %>% 
  mutate(moratorium = ifelse(row_placement > 3, "Effect of Game Day + In Moratorium", "Effect of Game Days")) %>% 
  mutate(moratorium = factor(moratorium, levels = c("Effect of Game Days", "Effect of Game Day + In Moratorium"))) %>% 
  mutate(type = factor(type, levels = c("All Games", "Home Game","Away Game", "All Games\nx\nIn Moratorium",
                                        "Home Game\nx\nIn Moratorium", "Away Game\nx\nIn Moratorium"))) %>% 
  ggplot(aes(row_number, estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax= conf.high)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark red") +
  facet_wrap(~moratorium) +
  scale_x_continuous(breaks = c(1:6), labels = c("All Games", "Home Game","Away Game", "All Games\nx\nIn Moratorium",
                                                 "Home Game\nx\nIn Moratorium", "Away Game\nx\nIn Moratorium")) +
  labs(x = " ", y = "", title = "Panel B: Sexual Assaults") +
  theme_minimal() 

result <- alc_game + sex_game + plot_layout(ncol = 1) 

game_moratorium_plot <- patchwork::patchworkGrob(result)

                  
                  