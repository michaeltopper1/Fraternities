
##### This file creates a average rape time trend based on municipality type


library(tidyverse)


monthly_rape <- ucr_master %>% 
  mutate(month = lubridate::month(month)) %>%  
  group_by(month, university, subtype2) %>% 
  summarize(avg_rape = mean(actual_rape_total, na.rm = T)) %>% 
  group_by(month, subtype2) %>% 
  summarize(month_avg = mean(avg_rape, na.rm = T))
  
monthly_rape %>% 
  mutate(subtype2 = ifelse(subtype2 == "(011) Four-year university", "University Police", "Local Municipality")) %>% 
  mutate(month = month.abb[month]) %>% 
  ggplot() + 
  geom_path(aes(x = month, y = month_avg, color = as.factor(subtype2), group = subtype2)) +
  geom_point(aes(x = month, y = month_avg, color = as.factor(subtype2))) +
  labs(color = "Municipality Type")+
  scale_x_discrete(limits = month.abb) +
  xlab("") +
  ylab("Average Rape")+
  theme_light() 

