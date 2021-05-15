## Purpose of script: creates raw data graphs that show the moratorium by university
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-10
##

library(tidyverse)

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv",
                        guess_max = 50000)
treated_universities <- daily_crime %>% 
  filter(!(university %in% ifc::untreated_universities()))

raw_graph_totals <- function(data, offense) {
  plot <- data %>% 
    mutate(university = case_when(
      university == "Louisiana State University and Agricultural & Mechanical College" ~ "Louisiana State University",
      university == "California Polytechnic State University-San Luis Obispo" ~ "Cal Poly San Luis Obispo",
      university == "University of Pittsburgh-Pittsburgh Campus" ~ "University of Pittsburgh",
      TRUE ~as.character(university)
    )) %>% 
    ggplot(aes(date, {{offense}})) +
    geom_path(aes(group = 1), color = "red") +
    geom_rect(aes(xmin = as.Date(closure_1), xmax = as.Date(closure_1_end), ymin = 0, ymax = Inf), fill = "light blue", alpha = 0.01) +
    geom_rect(aes(xmin = as.Date(closure_2), xmax = as.Date(closure_2_end), ymin = 0, ymax = Inf), fill = "light blue", alpha = 0.01) +
    facet_wrap(~university) +
    ggthemes::theme_fivethirtyeight()
  return(plot)
}

raw_sex <- raw_graph_totals(treated_universities, sexual_assault)
raw_alc <- raw_graph_totals(treated_universities, alcohol_offense)
raw_drug <- raw_graph_totals(treated_universities, drug_offense)
