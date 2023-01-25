library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)
options(modelsummary_model_labels="model")


if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv") 
}

rankings <- read_csv("data/rankings_2023_niche.csv") %>% 
  select(-rank)

## binning the last ones together
rankings <- rankings %>% 
  mutate(rank_adjust = ifelse(is.na(rank_adjust), 301, rank_adjust))

## getting the pre-moratorium average of alcohol offenses per 25 by university
avg_alc_pre <- daily_crime %>% 
  group_by(university) %>% 
  filter(date < closure_1) %>% 
  mutate(avg_alc_pre = mean(alcohol_offense_per25, na.rm = T)) %>% 
  select(university, avg_alc_pre) %>% 
  distinct() %>% ungroup()

daily_crime <- daily_crime %>% 
  left_join(avg_alc_pre) %>% 
  left_join(rankings, by = c("university" = "university_match")) %>% 
  mutate(ranking_treatment = treatment * rank_adjust)

daily_crime_weekends <- daily_crime_weekends %>% 
  left_join(avg_alc_pre) %>% 
  left_join(rankings, by = c("university" = "university_match")) %>% 
  mutate(ranking_treatment = treatment * rank_adjust)

daily_crime_weekdays <- daily_crime_weekdays %>% 
  left_join(avg_alc_pre) %>% 
  left_join(rankings, by = c("university" = "university_match")) %>% 
  mutate(ranking_treatment = treatment * rank_adjust)


# quartiles/quantiles ----------------------------------------------------

ifc_alc_quartiles <- quantile(daily_crime$avg_alc_pre) 

create_quartiles_ranking <- . %>% 
  mutate(alc_quartile_1 = ifelse(avg_alc_pre <= ifc_alc_quartiles[[2]], 1, 0),
         alc_quartile_2 = ifelse(avg_alc_pre <= ifc_alc_quartiles[[3]] & avg_alc_pre > ifc_alc_quartiles[[2]], 1, 0),
         alc_quartile_3 = ifelse(avg_alc_pre <= ifc_alc_quartiles[[4]] & avg_alc_pre > ifc_alc_quartiles[[3]], 1, 0),
         alc_quartile_4 = ifelse(avg_alc_pre > ifc_alc_quartiles[[4]], 1, 0))



daily_crime <- daily_crime %>% 
  create_quartiles_ranking()

daily_crime_weekends <- daily_crime_weekends %>% 
  create_quartiles_ranking() 

daily_crime_weekdays <- daily_crime_weekdays %>% 
  create_quartiles_ranking() 



data_list <-  list("All Days" = daily_crime, "Weekends" = daily_crime_weekends, "Weekdays" = daily_crime_weekdays)




map_df(data_list, ~feols(alcohol_offense_per25 ~
                           treatment:alc_quartile_1 + 
                           treatment:alc_quartile_2 + 
                           treatment:alc_quartile_3 +
                           treatment:alc_quartile_4| 
                           day_of_week^alc_quartile_1^alc_quartile_2^alc_quartile_3^alc_quartile_4 + 
                           university_by_academic_year^alc_quartile_1^alc_quartile_2^alc_quartile_3^alc_quartile_4 + 
                           holiday^alc_quartile_1^alc_quartile_2^alc_quartile_3^alc_quartile_4 + 
                           spring_semester^alc_quartile_1^alc_quartile_2^alc_quartile_3^alc_quartile_4 + 
                           game_occurred^alc_quartile_1^alc_quartile_2^alc_quartile_3^alc_quartile_4,
                         cluster = ~university, data =.x) %>% 
         broom::tidy(conf.int = T), .id = "var") %>% 
  mutate(var = factor(var, c("All Days", "Weekends", "Weekdays"))) %>% 
  ggplot(aes(term, estimate, group = var)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
  geom_point() +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_line(linetype = "dashed") +
  scale_x_discrete(labels = c("Q1\nLess\nPre-Moratorium\nOffenses", "Q2", "Q3", "Q4\nMore\nPre-Moratorium\nOffenses")) +
  facet_wrap(~var) +
  labs(x = "Pre-Treatment Mean Quartile", 
       y = "Point Estimate and 95% Confidence Interval") +
  theme_minimal()

map_df(data_list, ~feols(sexual_assault_per25 ~
                           treatment:alc_quartile_1 + 
                           treatment:alc_quartile_2 + 
                           treatment:alc_quartile_3 +
                           treatment:alc_quartile_4| 
                           day_of_week^alc_quartile_1^alc_quartile_2^alc_quartile_3^alc_quartile_4 + 
                           university_by_academic_year^alc_quartile_1^alc_quartile_2^alc_quartile_3^alc_quartile_4 + 
                           holiday^alc_quartile_1^alc_quartile_2^alc_quartile_3^alc_quartile_4 + 
                           spring_semester^alc_quartile_1^alc_quartile_2^alc_quartile_3^alc_quartile_4 + 
                           game_occurred^alc_quartile_1^alc_quartile_2^alc_quartile_3^alc_quartile_4,
                         cluster = ~university, data =.x) %>% 
         broom::tidy(conf.int = T), .id = "var") %>% 
  mutate(var = factor(var, c("All Days", "Weekends", "Weekdays"))) %>% 
  ggplot(aes(term, estimate, group = var)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
  geom_point() +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_line(linetype = "dashed") +
  scale_x_discrete(labels = c("Q1\nLess\nPre-Moratorium\nOffenses", "Q2", "Q3", "Q4\nMore\nPre-Moratorium\nOffenses")) +
  facet_wrap(~var) +
  labs(x = "Pre-Treatment Mean Quartile", 
       y = "Point Estimate and 95% Confidence Interval") +
  theme_minimal()
