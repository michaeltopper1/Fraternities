## Purpose of script: Graph of moratoria over time
##
## Author: Michael Topper
##
## Date Last Edited: 2021-04-15
##

library(tidyverse)

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv", guess_max = 50000)

closure_1 <- daily_crime %>% 
  distinct(closure_1, university, university_enacted_1) %>% 
  rename("closure" = closure_1, "reason" = university_enacted_1)
closure_2 <- daily_crime %>% 
  distinct(university, closure_2, university_enacted_2) %>% 
  rename("closure"= closure_2, "reason" = university_enacted_2)
closures <- bind_rows(closure_1, closure_2)


closure_distribution <- closures %>% 
  filter(!is.na(closure)) %>% 
  mutate(month = month(closure), year = year(closure)) %>% 
  mutate(date = mdy(paste0(month, '-1-', year))) %>% 
  mutate(n = 1) %>% 
  group_by(month, year) %>% 
  summarize(reason = sum(reason, na.rm = T), n = sum(n)) %>% 
  mutate(date = mdy(paste0(month, '-1-', year))) %>% 
  ggplot(aes(date, n)) +
  geom_col(fill = "red", alpha = 0.5) +
  scale_x_date(breaks ="4 month", date_labels = "%b/%Y") +
  labs(y = "", x = "") +
  theme_light()
