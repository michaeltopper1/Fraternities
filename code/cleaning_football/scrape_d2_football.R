
library(rvest)
library(tidyverse)


seasons <- c(2014:2019)

## clean data
clean_data <- function(link, name,  season) {
  cleaned <- read_html(paste0(link, season)) %>% 
    html_elements(".Card") %>% 
    html_table() %>% 
    pluck(1) %>% 
    janitor::row_to_names(row_number = 2) %>% 
    janitor::clean_names() %>% 
    extract(result, into = c("win_loss"), "(^[WL])") %>% 
    separate(date, c("day", "date"), sep = ",\\s") %>% 
    mutate(date = lubridate::mdy(paste0(date, ", ", season))) %>% 
    select(1:4) %>% 
    mutate(school = name) %>% 
    mutate(home = ifelse(str_detect(opponent, "^@"), 0 ,1)) %>% 
    mutate(win = ifelse(win_loss == "W",1 ,0)) %>% 
    rename(game_date = date)
  return(cleaned)
}


cal_poly <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/13/season/", "California Polytechnic State University-San Luis Obispo",.))

monmouth <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/2405/season/", "Monmouth University",.))

murray_state <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/93/season/", "Murray State University",.))

tufts <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/112/season/", "Tufts University", .))

albany_state <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/2013/season/", "Albany State University", .))

depauw <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/83/season/", "DePauw University", .))

## issue bowl game
james_madison <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/256/season/", "James Madison University", .))

## issue - bowl game
north_carolina_central <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/2428/season/", "North Carolina Central University", .))

praire <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/2504/season/", "Prairie View A & M University", .))

delaware <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/48/season/", "University of Delaware", .))

delaware_state <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/2169/season/", "Delaware State University", .))

ferrum <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/366/season/", "Ferrum College", .))

hampden <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/297/season/", "Hampden-Sydney College", .))

suny_albany <- map_df(seasons, ~clean_data("https://www.espn.com/college-football/team/schedule/_/id/399/season/", "SUNY at Albany", .))

d2_football <- rbind(cal_poly, monmouth, murray_state, tufts,
                     albany_state,depauw, james_madison, north_carolina_central, 
                     praire, delaware, delaware_state, ferrum, hampden, suny_albany)

d2_football <- d2_football %>% 
  filter(!is.na(game_date))

d2_football %>% 
  write_csv("created_data/football/football_schools_d2.csv")

# 
# daily_crime_all %>% 
#   left_join(d2_football, by= c("university" = "school", "date" = "game_date", "win_loss" = "win_loss", "home_game" = "home", "win" = "win")) %>% 
#   group_by(university) %>% 
#   count(win_loss) %>% View()
