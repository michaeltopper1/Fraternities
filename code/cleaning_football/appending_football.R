## Purpose of script: Harmonize the two football datasets
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-14
##

library(tidyverse)

d1 <- read_csv("created_data/football/football_schools.csv")
d2 <- read_csv("created_data/football/football_schools_d2.csv") %>% 
  mutate(game_occurred = 1) %>% 
  mutate(away_game = ifelse(home == 0, 1, 0))

d1 <- d1 %>% 
  select(school,game_date, opponent, win, home_game, away_game, game_occurred)
d2 <- d2 %>% 
  select(school,game_date, opponent, win, home_game = home, away_game, game_occurred)

## filtering out games that were canceled or postponed.
football_final <- rbind(d1, d2) %>% 
  filter(!is.na(win))


football_final %>% 
  write_csv("created_data/xmaster_data/football_final.csv")

