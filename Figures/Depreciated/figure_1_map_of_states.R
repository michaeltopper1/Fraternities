
## graph of states that are used in the analysis
library(tidyverse)
library(sf)

states_map <-  st_read("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/Map_data/cb_2018_us_state_20m.shp")
mapview::mapview(states_map)

states_map <- janitor::clean_names(states_map)
states <- c("KS","MO","WV","OH","SC","ID","TX","MI","LA","IA","IL","WA","VA","KY")

states_map <- states_map %>% 
  mutate(states_in = ifelse(stusps %in% states, 1, 0))

X11(type = "cairo")
states_map %>% 
  ggplot() + 
  geom_sf(aes(fill = as.factor(states_in) ), alpha = 0.7) + 
  scale_fill_manual(values = c("#999281", "#E41A1C"))+
  coord_sf(xlim = c(-125, -60), ylim = c(23, 50)) +
  labs(fill = "State in Sample", alpha = "") +
  theme(legend.position="none", panel.background = element_rect(fill = 'white', colour = 'white')) 
