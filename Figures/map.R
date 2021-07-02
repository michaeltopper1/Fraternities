## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-06-24
##

library(tidyverse)
library(maps)


daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv")
ifc_fraternities <- read_csv("Data/ifc_fraternity_count.csv")

us_states <- map_data("state") %>% 
  as_tibble()

universities <- daily_crime %>% 
  mutate(ever_treated = ifelse(university %in% ifc::untreated_universities(), "No Moratorium", "Experienced Moratorium")) %>% 
  distinct(university, ever_treated, longitude, latitude) %>% 
  left_join(ifc_fraternities)
map_of_schools <- us_states %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "white") +
  borders("state") +
  geom_point(data = universities, aes(longitude, latitude, group = university, color = as_factor(ever_treated), size = most_recent)) +
  labs(color = "", size = "IFC size") +
  ggthemes::theme_map() +
  scale_size_binned()
