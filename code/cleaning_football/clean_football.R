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
    school == "Arizona" ~ "University of Arizona" ,
    school == "Arkansas State" ~ "Arkansas State University-Main Campus",
    school == "Arkansas" ~ "University of Arkansas" , 
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
    school == "Illinois" ~"University of Illinois Urbana-Champaign" , 
    school == "Indiana" ~ "Indiana University-Bloomington", 
    school == "Iowa" ~ "University of Iowa", 
    school == "Kansas" ~ "University of Kansas", 
    school == "Kentucky" ~ "University of Kentucky" , 
    school == "LSU" ~ "Louisiana State University and Agricultural & Mechanical College",
    school == "Louisiana" ~ "University of Louisiana at Lafayette",
    school == "Marshall" ~ "Marshall University", 
    school == "Michigan" ~"University of Michigan-Ann Arbor", 
    school == "Michigan State" ~ "Michigan State University" , 
    school == "Missouri" ~ "University of Missouri-Columbia", 
    school == "Ole Miss" ~ "University of Mississippi" , 
    school == "Nebraska" ~ "University of Nebraska-Lincoln" , 
    school == "Nevada" ~ "University of Nevada-Reno", 
    school == "New Mexico" ~ "University of New Mexico-Main Campus", 
    school == "North Carolina State" ~ "North Carolina State University at Raleigh", 
    school == "Ohio" ~ "Ohio University-Main Campus", 
    school == "Ohio State" ~ "Ohio State University-Main Campus" , 
    school == "Pitt" ~ "University of Pittsburgh-Pittsburgh Campus",
    school == "Rutgers" ~ "Rutgers University-New Brunswick",
    school == "San Diego State" ~ "San Diego State University" ,
    school == "South Carolina" ~ "University of South Carolina-Columbia", 
    school == "SMU" ~ "Southern Methodist University", 
    school == "Syracuse" ~ "Syracuse University", 
    school == "Texas A&M" ~ "Texas A & M University-College Station", 
    school == "Texas" ~ "The University of Texas at Austin" , 
    school == "TCU" ~ "Texas Christian University", 
    school == "Texas State" ~ "Texas State University",
    school == "Texas Tech" ~ "Texas Tech University" , 
    school == "USC" ~ "University of Southern California", 
    school == "Virginia" ~ "University of Virginia-Main Campus",
    school == "Washington State" ~ "Washington State University" ,
    school == "West Virginia"~ "West Virginia University",
    school == "Wisconsin" ~ "University of Wisconsin-Madison" , 
  )) 


football_schools <- football_schools %>% 
  mutate(game_date = lubridate::ymd(game_date)) %>% 
  mutate(home_game = ifelse(is.na(home_away), 1, 0)) %>% 
  mutate(away_game = ifelse(str_detect(home_away, "@"), 1, 0)) %>% 
  mutate(away_game = ifelse(is.na(away_game), 0, away_game)) %>% 
  select(-cmp, -att, -pct, -yds, -td, - game_number, - rank) %>% 
  rename(win_loss = x7) %>% 
  mutate(game_occurred = 1)


football_schools %>% 
  write_csv("created_data/football/football_schools.csv")
