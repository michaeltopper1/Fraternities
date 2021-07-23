## Purpose of script: creates raw data graphs that show the moratorium by university
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-10
##

library(tidyverse)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv")
}

treated_universities <- daily_crime %>% 
  filter(!(university %in% ifc::untreated_universities())) %>% 
  mutate(university = ifelse(university == "Louisiana State University and Agricultural & Mechanical College", "Louisiana State University", university)) %>% 
  mutate(university = ifelse(university == "California Polytechnic State University-San Luis Obispo", "Cal Poly San Luis Obispo", university)) %>%
  mutate(university = ifelse(university == "University of Pittsburgh-Pittsburgh Campus", "University of Pittsburgh", university)) %>%
  mutate(university = gsub("-Main Campus", "", university)) %>% 
  mutate(university = ifelse(university == "North Carolina State University at Raleigh", "North Carolina State", university))

raw_graph_totals <- function(data, offense) {
  plot <- data %>% 
    mutate(university = case_when(
      university == "Louisiana State University and Agricultural & Mechanical College" ~ "Louisiana State University",
      university == "California Polytechnic State University-San Luis Obispo" ~ "Cal Poly San Luis Obispo",
      university == "University of Pittsburgh-Pittsburgh Campus" ~ "University of Pittsburgh",
      TRUE ~as.character(university)
    )) %>% 
    ggplot(aes(date, {{offense}})) +
    geom_path(aes(group = 1), color = "black", alpha = 0.7) +
    geom_rect(aes(xmin = as.Date(closure_1), xmax = as.Date(closure_1_end), ymin = 0, ymax = Inf), fill = "light blue", alpha = 0.01) +
    geom_rect(aes(xmin = as.Date(closure_2), xmax = as.Date(closure_2_end), ymin = 0, ymax = Inf), fill = "light blue", alpha = 0.01) +
    facet_wrap(~university, ncol = 6, scales = "free") +
    theme_minimal() 
  return(plot)
}

raw_sex <- raw_graph_totals(treated_universities, sexual_assault) + 
  labs(x = "", y  = "") 
raw_alc <- raw_graph_totals(treated_universities, alcohol_offense) + 
  labs(x = "", y  = "")
raw_drug <- raw_graph_totals(treated_universities, drug_offense)+ 
  labs(x = "", y  = "")
raw_robbery <- raw_graph_totals(treated_universities, robbery_burglary)+ 
  labs(x = "", y  = "")
