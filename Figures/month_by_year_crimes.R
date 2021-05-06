## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-04-30
##

library(tidyverse)
library(lubridate)
library(glue)
library(ifc)

## using all months including summer 
path <- "Created Data/xMaster_data_2021/daily_panel.csv"
daily_crime <- read_csv(path, guess_max = 50000)

month_by_year_crimes <- daily_crime %>% 
  group_by(month, year) %>% 
  summarize(across(c("sexual_assault", "alcohol_offense", "drug_offense"), ~mean(., na.rm = T))) %>%
  mutate(month_year = mdy(paste(month, "/1/", year))) %>% 
  ungroup() %>% 
  select(-month, -year) %>% 
  pivot_longer(cols = -c(month_year), names_to = "offense", values_to = "average") %>% 
  arrange(month_year) %>% 
  ggplot(aes(month_year, average)) +
  geom_point(aes(group = offense, color = offense)) +
  geom_path(aes(group = offense, color = offense)) +
  labs(y = "Monthly Averages", x = "", color = "Offense Type") +
  scale_x_date(date_breaks = "month", date_labels = "%b-%y") +
  scale_color_discrete(breaks = c("alcohol_offense", "drug_offense", "sexual_assault"), labels = c("Alcohol", "Drug", "Sexual Assault")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")


