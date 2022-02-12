## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-11
##

library(tidyverse)

fb_schools <- list.files(path = "data/football_games/")

football_schools <- map_df(fb_schools, ~read_csv(paste0("data/football_games/", .)))
football_schools <- football_schools %>% 
  janitor::clean_names() %>% 
  rename("rank" = "rk", game_number = g_number, home_away = x5, game_date = date) %>% 
  mutate(win = ifelse(x7 == "W", 1, 0))


football_schools <- football_schools %>% 
  mutate(school = case_when(
    school == "Arkansas State" ~ "Arkansas State University-Main Campus",
    school == "Ball State" ~ "Ball State University"  , 
    school == "California" ~ "University of California-Berkeley",
    school == "Buffalo" ~"University at Buffalo" ,
    school == "UCF" ~ "University of Central Florida"  ,
    school == "Clemson" ~ "Clemson University" ,
    school == "East Carolina" ~ "East Carolina University", 
    school == "Florida Atlantic" ~ "Florida Atlantic University",
    school == "Florida International" ~ "Florida International University", 
    school == "Florida State" ~ "Florida State University", 
    school == "Idaho" ~ "University of Idaho" , 
    school == "Indiana" ~ "Indiana University-Bloomington", 
    school == "Iowa" ~ "University of Iowa", 
    school == "Kansas" ~ "University of Kansas", 
    school == "LSU" ~ "Louisiana State University and Agricultural & Mechanical College",
    school == "Marshall" ~ "Marshall University", 
    school == "Michigan" ~"University of Michigan-Ann Arbor", 
    school == "Missouri" ~ "University of Missouri-Columbia", 
    school == "New Mexico" ~ "University of New Mexico-Main Campus", 
    school == "North Carolina State" ~ "North Carolina State University at Raleigh", 
    school == "Ohio" ~ "Ohio University-Main Campus", 
    school == "Ohio State" ~ "Ohio State University-Main Campus" , 
    school == "Pitt" ~ "University of Pittsburgh-Pittsburgh Campus",
    school == "Rutgers" ~ "Rutgers University-New Brunswick",
    school == "San Diego State" ~ "San Diego State University" ,
    school == "Syracuse" ~ "Syracuse University", 
    school == "Texas State" ~ "Texas State University",
    school == "Virginia" ~ "University of Virginia-Main Campus",
    school == "Washington State" ~ "Washington State University" ,
    school == "West Virginia"~ "West Virginia University"
  )) 

football_schools <- football_schools %>% 
  mutate(game_date = lubridate::ymd(game_date)) %>% 
  mutate(home_game = ifelse(is.na(home_away), 1, 0)) %>% 
  mutate(away_game = ifelse(str_detect(home_away, "@"), 1, 0)) %>% 
  mutate(away_game = ifelse(is.na(away_game), 0, away_game)) %>% 
  select(-cmp, -att, -pct, -yds, -td, - game_number, - rank) %>% 
  rename(win_loss = x7)


football_schools %>% 
  write_csv("created_data/xmaster_data/football_schools.csv")
