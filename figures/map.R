## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-06-24
##

library(tidyverse)
library(maps)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}

ifc_fraternities <- read_csv("data/ifc_fraternity_count.csv") %>% 
  filter(university %in% ifc::moratorium_schools())

us_states <- map_data("state") %>% 
  as_tibble()

detach("package:maps", unload = TRUE)

ipeds <- read_csv("created_data/ipeds/ipeds_final.csv") %>% 
  filter(university %in% ifc::moratorium_schools())
ipeds <- ipeds %>% 
  group_by(university) %>% 
  summarize(avg_enrollment = mean(undergraduate_enrollment, na.rm = T))

universities <- daily_crime %>% 
  select(university, latitude, longitude, control_of_institution) %>% 
  distinct(university, latitude, longitude, control_of_institution, .keep_all = T) %>% 
  left_join(ifc_fraternities) %>% 
  left_join(ipeds) %>% 
  filter(university %in% ifc::moratorium_schools()) %>% 
  distinct(university,.keep_all =  T)

universities <- universities %>% 
  mutate(ifc_frac = most_recent/avg_enrollment) 

# universities %>% 
#   select(ifc_frac) %>% 
#   modelsummary::datasummary_skim(fmt = "%.4f")
  
## 5 schools had missing data for IFC
map_of_schools <- us_states %>% 
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(fill = "white") +
  borders("state") +
  geom_point(data = universities %>% filter(!is.na(ifc_frac)), aes(longitude, latitude, group = university, size = ifc_frac, color = control_of_institution),
             alpha = 0.7) +
  geom_point(data = universities %>% filter(is.na(ifc_frac)), aes(longitude, latitude, group = university, color = control_of_institution),
             alpha =0.5) +
  labs(color = "University Type:", size = "Fraction of total enrollment in IFC Fraternities:") +
  ggthemes::theme_map() +
  theme(legend.position = "bottom", legend.box="vertical", legend.margin=margin())
