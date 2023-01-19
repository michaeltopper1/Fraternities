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


daily_crime <- daily_crime %>% 
  left_join(rankings, by = c("university" = "university_match")) %>% 
  mutate(ranking_treatment = treatment * rank_adjust)

daily_crime_weekends <- daily_crime_weekends %>% 
  left_join(rankings, by = c("university" = "university_match")) %>% 
  mutate(ranking_treatment = treatment * rank_adjust)

daily_crime_weekdays <- daily_crime_weekdays %>% 
  left_join(rankings, by = c("university" = "university_match")) %>% 
  mutate(ranking_treatment = treatment * rank_adjust)


# quartiles/quantiles ----------------------------------------------------

ifc_rank_quartiles <- quantile(rankings$rank_adjust) 
ifc_rank_quantiles <- quantile(rankings$rank_adjust, probs = c(0,.33,.66,1))


create_quantiles_ranking <- . %>% 
  mutate(rank_quantile_1 = ifelse(rank_adjust <= ifc_rank_quantiles[[2]], 1, 0),
         rank_quantile_2 = ifelse(rank_adjust <= ifc_rank_quantiles[[3]] & rank_adjust > ifc_rank_quantiles[[2]], 1, 0),
         rank_quantile_3 = ifelse(rank_adjust <= ifc_rank_quantiles[[4]] & rank_adjust > ifc_rank_quantiles[[3]], 1, 0))


create_quartiles_ranking <- . %>% 
  mutate(rank_quartile_1 = ifelse(rank_adjust <= ifc_rank_quartiles[[2]], 1, 0),
         rank_quartile_2 = ifelse(rank_adjust <= ifc_rank_quartiles[[3]] & rank_adjust > ifc_rank_quartiles[[2]], 1, 0),
         rank_quartile_3 = ifelse(rank_adjust <= ifc_rank_quartiles[[4]] & rank_adjust > ifc_rank_quartiles[[3]], 1, 0),
         rank_quartile_4 = ifelse(rank_adjust > ifc_rank_quartiles[[4]], 1, 0))

daily_crime <- daily_crime %>% 
  create_quartiles_ranking() %>% 
  create_quantiles_ranking()

daily_crime_weekends <- daily_crime_weekends %>% 
  create_quartiles_ranking() %>% 
  create_quantiles_ranking()

daily_crime_weekdays <- daily_crime_weekdays %>% 
  create_quartiles_ranking() %>% 
  create_quantiles_ranking()



data_list <-  list("All Days" = daily_crime, "Weekends" = daily_crime_weekends, "Weekdays" = daily_crime_weekdays)

modelsummary(feols(c(alcohol_offense_per25,sexual_assault_per25) ~
                     treatment:rank_quantile_1 + 
                     treatment:rank_quantile_2 + 
                     treatment:rank_quantile_3 | day_of_week + university_by_academic_year + holiday + spring_semester + game_occurred,
                   cluster = ~university, data =daily_crime),
             stars = T)

map_df(data_list, ~feols(alcohol_offense_per25 ~
                           treatment:rank_quartile_1 + 
                           treatment:rank_quartile_2 + 
                           treatment:rank_quartile_3 +
                           treatment:rank_quartile_4| day_of_week + university_by_academic_year + holiday + spring_semester + game_occurred,
                         cluster = ~university, data =.x) %>% 
         broom::tidy(conf.int = T), .id = "var") %>% 
  mutate(var = factor(var, c("All Days", "Weekends", "Weekdays"))) %>% 
  ggplot(aes(term, estimate, group = var)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
  geom_point() +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_line(linetype = "dashed") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  facet_wrap(~var) +
  labs(x = "Quantile", y = "Point Estimate and 95% Confidence Interval") +
  theme_minimal()


map(data_list, ~feols(c(alcohol_offense_per25,sexual_assault_per25) ~
                           treatment:rank_quantile_1 + 
                           treatment:rank_quantile_2 + 
                           treatment:rank_quantile_3 | day_of_week + university_by_academic_year + holiday + spring_semester + game_occurred,
                         cluster = ~university, data =.x))
map_df(data_list, ~feols(alcohol_offense_per25 ~
                           treatment:rank_quantile_1 + 
                           treatment:rank_quantile_2 + 
                           treatment:rank_quantile_3 | day_of_week + university_by_academic_year + holiday + spring_semester + game_occurred,
                         cluster = ~university, data =.x) %>% 
         broom::tidy(conf.int = T), .id = "var") %>% 
  mutate(var = factor(var, c("All Days", "Weekends", "Weekdays"))) %>% 
  ggplot(aes(term, estimate, group = var)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
  geom_point() +
  geom_hline(yintercept = 0, color = "dark red") +
  # geom_line(linetype = "dashed") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  facet_wrap(~var) +
  labs(x = "Quantile", y = "Point Estimate and 95% Confidence Interval",
       title = "Alcohol Offense (Treciles)") +
  theme_minimal()

map_df(data_list, ~feols(sexual_assault_per25 ~
                           treatment:rank_quantile_1 + 
                           treatment:rank_quantile_2 + 
                           treatment:rank_quantile_3 | day_of_week + university_by_academic_year + holiday + spring_semester + game_occurred,
                         cluster = ~university, data =.x) %>% 
         broom::tidy(conf.int = T), .id = "var") %>% 
  mutate(var = factor(var, c("All Days", "Weekends", "Weekdays"))) %>% 
  ggplot(aes(term, estimate, group = var)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
  geom_point() +
  geom_hline(yintercept = 0, color = "dark red") +
  # geom_line(linetype = "dashed") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  facet_wrap(~var) +
  labs(x = "Quantile", y = "Point Estimate and 95% Confidence Interval",
       title = "Sexual Assault (Treciles)") +
  theme_minimal()

map_df(data_list, ~feols(alcohol_offense_per25 ~
                           treatment:rank_quartile_1 + 
                           treatment:rank_quartile_2 + 
                           treatment:rank_quartile_3 +
                           treatment:rank_quartile_4| day_of_week + university_by_academic_year + holiday + spring_semester + game_occurred,
                         cluster = ~university, data =.x) %>% 
         broom::tidy(conf.int = T), .id = "var") %>% 
  mutate(var = factor(var, c("All Days", "Weekends", "Weekdays"))) %>% 
  ggplot(aes(term, estimate, group = var)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
  geom_point() +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_line(linetype = "dashed") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  facet_wrap(~var) +
  labs(x = "Greek Life Ranking Quartile", y = "Point Estimate and 95% Confidence Interval",
       title = "Alcohol Offenses") +
  theme_minimal()
map_df(data_list, ~feols(sexual_assault_per25 ~
                           treatment:rank_quartile_1 + 
                           treatment:rank_quartile_2 + 
                           treatment:rank_quartile_3 +
                           treatment:rank_quartile_4| day_of_week + university_by_academic_year + holiday + spring_semester + game_occurred,
                         cluster = ~university, data =.x) %>% 
         broom::tidy(conf.int = T), .id = "var") %>% 
  mutate(var = factor(var, c("All Days", "Weekends", "Weekdays"))) %>% 
  ggplot(aes(term, estimate, group = var)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
  geom_point() +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_line(linetype = "dashed") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  facet_wrap(~var) +
  labs(x = "Greek Life Ranking Quartile", y = "Point Estimate and 95% Confidence Interval",
       title = "Sexual Assaults") +
  theme_minimal()
