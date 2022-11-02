library(tidyverse)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv") 
}

daily_crime %>% 
  distinct(university, total_enrollment, year, undergraduate_enrollment) %>% 
  group_by(university) %>% 
  summarize(total_enrollment_mean = mean(total_enrollment,na.rm = T),
            undergraduate_enrollment_mean = mean(undergraduate_enrollment, na.rm = T)) %>%
  mutate(university = fct_reorder(university, total_enrollment_mean)) %>% 
  ggplot(aes(university, total_enrollment_mean)) +
  geom_col() +
  labs(x = "Universities", y = "Mean of Total Enrollment (2014-2019)") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) 
  
