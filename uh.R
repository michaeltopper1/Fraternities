library(tidyverse)
library(lubridate)

nectir <- read_csv("/Users/michaeltopper/Desktop/nectir.csv") %>% 
  janitor::clean_names()

nectir <- nectir %>% 
  mutate(created_at = with_tz(created_at, tz = "America/Los_Angeles")) %>% 
  mutate(time_created = hms::as_hms(created_at),
         hour_created = hour(time_created),
         date_created = as_date(created_at),
         week_created = week(date_created)) %>% 
  mutate(week_created = week_created - 37) %>% 
  mutate(week_created = paste("Week", week_created)) 

nectir %>% 
  mutate(day_created = wday(date_created, label = T)) %>% 
  count(day_created, week_created) %>% 
  ggplot(aes(day_created,n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.6 ) +
  facet_wrap(~week_created) +
  labs(x = "", y = "Number of Nectir Messages", title = "Number of Nectir Messages Each Week of the Quarter",
       subtitle = "Econ 145: Fall 2022") +
  theme_minimal()


nectir %>% 
  mutate(day_created = wday(date_created, label = T)) %>% 
  count(day_created, week_created, date_created) %>% 
  arrange(day_created) %>% 
  group_by(day_created) %>% 
  mutate(growth = lead(n) -n) %>%
  mutate(negative = ifelse(growth < 0, "1", "0")) %>% 
  ggplot(aes(day_created, growth, color = negative)) +
  geom_point() +
  geom_segment(aes(x = day_created, xend = day_created, y = 0, yend = growth)) +
  scale_color_manual(values = c("black", "red")) +
  geom_text(aes(label = n), vjust = -.6) +
  # scale_x_date(date_breaks = "day", date_labels = "%a") +
  facet_wrap(~week_created) +
  theme_minimal()
  
fantano %>% 
  select(user_score)