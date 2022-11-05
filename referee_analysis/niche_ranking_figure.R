## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-11-04
##

library(tidyverse)


rankings <- read_csv("data/rankings_2023_niche.csv") %>% 
  select(-rank)

niche_ranking_graph <- rankings %>% 
  mutate(median_rank = median(rank_adjust, na.rm = T)) %>% 
  mutate(rank_placement = case_when(
    rank_adjust <=  25 ~ "Top 25",
    rank_adjust <= 50 & rank_adjust > 25 ~ "Top 50",
    rank_adjust <= 100 & rank_adjust > 50 ~ "Top 100",
    rank_adjust <=200 & rank_adjust > 100 ~ "Top 200",
    rank_adjust <= 300 & rank_adjust > 200 ~ "Top 300",
    is.na(rank_adjust) ~ "Unranked"
  )) %>% 
  mutate(rank_placement = factor(rank_placement, levels =c("Top 25",
                                                          "Top 50",
                                                          "Top 100",
                                                          "Top 200",
                                                          "Top 300",
                                                          "Unranked"))) %>% 
  mutate(university = glue::glue("{university} ({rank_adjust})")) %>% 
  mutate(university = fct_reorder(university, rank_adjust)) %>% 
  ggplot(aes(x = university, y = rank_adjust)) +
  geom_point(aes(shape = rank_placement)) +
  geom_hline(aes(yintercept = median_rank), linetype = "dashed", color = "dark red") +
  labs(x = " ", y = "Niche.com Greek Life Ranking")  +
  facet_grid(.~rank_placement, scales = "free", switch = "x", space = "free_x") +
  annotate("segment",x=Inf,xend=-Inf,y=-Inf,yend=-Inf,color="black",lwd=1) +
  scale_y_continuous(breaks = c(0, 50, 64, 100, 150, 200, 250)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")
