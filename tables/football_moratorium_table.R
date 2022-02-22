## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-21
##

library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv") %>% 
    mutate(home_game = ifelse(is.na(home_game), 0, home_game)) 
}


football_moratorium_table <- daily_crime %>% 
  count(treatment, game_occurred) %>% 
  mutate(type = case_when(
    treatment == 1 & game_occurred == 1 ~ "Moratorium and Game Day",
    treatment == 1 & game_occurred == 0 ~ "Moratorium", 
    treatment == 0 & game_occurred == 1 ~ "Game Day", 
    treatment == 0 & game_occurred == 0 ~ "No Game No Moratorium"
  )) %>% 
  select(type, n) %>% 
  add_row(type = "Number of Schools with Football Games", n = 34) %>% 
  kbl(col.names = c("Day Type", n = "Number of Days"),
      caption = "\\label{football_moratorium_table}Distribution of Game Days and Moratoriums Over Sample Period") %>% 
  kable_styling() %>% 
  row_spec(4, hline_after = T) %>% 
  row_spec(5, bold = T) %>% 
  footnote(list("Schools without football games do not have a football team. Days represent academic-calendar days."), 
           threeparttable = T)
