#############################################################################
##  This file makes the map for the per-capita reports of rape by school.  ##
#############################################################################



load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Master_data/master_ucr_ipeds_cross.rda")
library(tidyverse)
library(maps)
usa = data.frame(map("state", plot = FALSE)[c("x",
                                                "y")])


## getting the averages by school
averages <- ucr_master %>% 
  group_by(university, year ) %>% 
  summarize(avg_rape = mean(rape_per_hundredthousand,na.rm = T)) %>% 
  summarize(avg_rape = mean(avg_rape))
  
ucr_new <- ucr_master %>% 
  inner_join(y = averages)

per_capita_map <- ucr_new %>% 
  distinct(university, avg_rape, longitude, latitude) %>% 
  ggplot() +
  geom_path(data = usa, aes(x, y)) +
  coord_map() +
  geom_point(data = ucr_new, aes(x = longitude, y = latitude, size = avg_rape, alpha = 0.8), shape = 23 ,color = "blue", alpha = 0.8) +
  labs(x = "", y = "",size = "Average of yearly reports of rape per-100k") +
  ggthemes::theme_map() +
  theme(legend.position="top",legend.background = element_rect(fill = alpha('white', 0.1)))+
  guides(shape = guide_legend(override.aes = list(size = 0.5)))

