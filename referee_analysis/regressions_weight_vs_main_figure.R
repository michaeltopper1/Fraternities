library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)
library(fwildclusterboot)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv") 
}


# creating fe columns for day of week to make it work with fboot ----------
daily_crime <- daily_crime %>% 
  mutate(day_of_week_fe = lubridate::wday(date)) 

daily_crime_weekends <- daily_crime_weekends %>% 
  mutate(day_of_week_fe = lubridate::wday(date)) 

daily_crime_weekdays <- daily_crime_weekdays %>% 
  mutate(day_of_week_fe = lubridate::wday(date)) 





# estimations -------------------------------------------------------------

explanatory_vars <- c("treatment")


# fixed effects for daily_level -------------------------------------------
fixed_effects_1 <- c("day_of_week_fe", "academic_year", "spring_semester", "university", "holiday", "game_occurred")
fixed_effects_2 <- c("day_of_week_fe", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")
fixed_effects_3 <- c("day_of_week_fe", "university_by_academic_year_by_semester", "holiday", "game_occurred")


# non-weighted estimations ------------------------------------------------

alc_1 <- feols(alcohol_offense_per25 ~ treatment | 
                 day_of_week_fe + academic_year + university + 
                 holiday + spring_semester + game_occurred,
               cluster = "university", 
               data = daily_crime)

alc_2 <- feols(alcohol_offense_per25 ~ treatment | 
                 day_of_week_fe + university_by_academic_year + 
                 holiday + spring_semester + game_occurred,
               cluster = "university", 
               data = daily_crime)

alc_3 <- feols(alcohol_offense_per25 ~ treatment | 
                 day_of_week_fe + university_by_academic_year_by_semester + 
                 holiday + game_occurred,
               cluster = "university", 
               data = daily_crime)

## column 4 weekends preferred
alc_4_weekend <- feols(alcohol_offense_per25 ~ treatment | 
                         day_of_week_fe + university_by_academic_year + 
                         holiday + spring_semester + game_occurred,
                       cluster = "university", 
                       data = daily_crime_weekends)

## column 5 weekdays preferred
alc_5_weekday <- feols(alcohol_offense_per25 ~ treatment | 
                         day_of_week_fe + university_by_academic_year + 
                         holiday + spring_semester + game_occurred,
                       cluster = "university", 
                       data = daily_crime_weekdays)



alc_main <- list(alc_1, alc_2, alc_3, alc_4_weekend, 
                 alc_5_weekday)

sex_1 <- feols(sexual_assault_per25 ~ treatment | 
                 day_of_week_fe + academic_year + university + 
                 holiday + spring_semester + game_occurred,
               cluster = "university", 
               data = daily_crime)

sex_2 <- feols(sexual_assault_per25 ~ treatment | 
                 day_of_week_fe + university_by_academic_year + 
                 holiday + spring_semester + game_occurred,
               cluster = "university", 
               data = daily_crime)


sex_3 <- feols(sexual_assault_per25 ~ treatment | 
                 day_of_week_fe + university_by_academic_year_by_semester + 
                 holiday + game_occurred,
               cluster = "university", 
               data = daily_crime)

## column 4 weekends preferred
sex_4_weekend <- feols(sexual_assault_per25 ~ treatment | 
                         day_of_week_fe + university_by_academic_year + 
                         holiday + spring_semester + game_occurred,
                       cluster = "university", 
                       data = daily_crime_weekends)

## column 5 weekdays preferred
sex_5_weekday <- feols(sexual_assault_per25 ~ treatment | 
                         day_of_week_fe + university_by_academic_year + 
                         holiday + spring_semester + game_occurred,
                       cluster = "university", 
                       data = daily_crime_weekdays)


# weighted estimations ----------------------------------------------------


alc_1_w <- feols(alcohol_offense_per25 ~ treatment | 
                 day_of_week_fe + academic_year + university + 
                 holiday + spring_semester + game_occurred,
               cluster = "university", 
               data = daily_crime,
               weights = daily_crime$total_enrollment)

alc_2_w <- feols(alcohol_offense_per25 ~ treatment | 
                 day_of_week_fe + university_by_academic_year + 
                 holiday + spring_semester + game_occurred,
               cluster = "university", 
               data = daily_crime,
               weights = daily_crime$total_enrollment)

alc_3_w <- feols(alcohol_offense_per25 ~ treatment | 
                 day_of_week_fe + university_by_academic_year_by_semester + 
                 holiday + game_occurred,
               cluster = "university", 
               data = daily_crime,
               weights = daily_crime$total_enrollment)

## column 4 weekends preferred
alc_4_weekend_w <- feols(alcohol_offense_per25 ~ treatment | 
                         day_of_week_fe + university_by_academic_year + 
                         holiday + spring_semester + game_occurred,
                       cluster = "university", 
                       data = daily_crime_weekends,
                       weights = daily_crime_weekends$total_enrollment)

## column 5 weekdays preferred
alc_5_weekday_w <- feols(alcohol_offense_per25 ~ treatment | 
                         day_of_week_fe + university_by_academic_year + 
                         holiday + spring_semester + game_occurred,
                       cluster = "university", 
                       data = daily_crime_weekdays,
                       weights = daily_crime_weekdays$total_enrollment)



sex_1_w <- feols(sexual_assault_per25 ~ treatment | 
                 day_of_week_fe + academic_year + university + 
                 holiday + spring_semester + game_occurred,
               cluster = "university", 
               data = daily_crime,
               weights = daily_crime$total_enrollment)

sex_2_w <- feols(sexual_assault_per25 ~ treatment | 
                 day_of_week_fe + university_by_academic_year + 
                 holiday + spring_semester + game_occurred,
               cluster = "university", 
               data = daily_crime,
               weights = daily_crime$total_enrollment)


sex_3_w <- feols(sexual_assault_per25 ~ treatment | 
                 day_of_week_fe + university_by_academic_year_by_semester + 
                 holiday + game_occurred,
               cluster = "university", 
               data = daily_crime,
               weights = daily_crime$total_enrollment)

## column 4 weekends preferred
sex_4_weekend_w <- feols(sexual_assault_per25 ~ treatment | 
                         day_of_week_fe + university_by_academic_year + 
                         holiday + spring_semester + game_occurred,
                       cluster = "university", 
                       data = daily_crime_weekends,
                       weights = daily_crime_weekends$total_enrollment)

## column 5 weekdays preferred
sex_5_weekday_w <- feols(sexual_assault_per25 ~ treatment | 
                         day_of_week_fe + university_by_academic_year + 
                         holiday + spring_semester + game_occurred,
                       cluster = "university", 
                       data = daily_crime_weekdays,
                       weights = daily_crime_weekdays$total_enrollment)


alc_main <- map_df(list(alc_1, alc_2, alc_3, alc_4_weekend, 
                 alc_5_weekday), 
                 ~.x %>% broom::tidy(conf.int = T) %>% mutate(model = "Unweighted",
                                                              outcome = "alcohol_offense")) %>% 
  mutate(row_number = row_number(),
         column = glue::glue("Column\n({row_number})"))
  

alc_main_w <- map_df(list(alc_1_w, alc_2_w, alc_3_w, alc_4_weekend_w, 
                   alc_5_weekday_w),
                   ~.x %>% broom::tidy(conf.int = T) %>% mutate(model = "Weighted",
                                                                outcome = "alcohol_offense",)) %>% 
  mutate(row_number = row_number(),
         column = glue::glue("Column\n({row_number})"))

sex_main <- map_df(list(sex_1, sex_2, sex_3, sex_4_weekend, sex_5_weekday),
                   ~.x %>% broom::tidy(conf.int = T) %>% mutate(model = "Unweighted",
                                                                outcome = "sexual_assault")) %>% 
  mutate(row_number = row_number(),
         column = glue::glue("Column\n({row_number})"))

sex_main_w <- map_df(list(sex_1_w, sex_2_w, sex_3_w, sex_4_weekend_w, sex_5_weekday_w),
                     ~.x %>% broom::tidy(conf.int = T) %>% mutate(model = "Weighted",
                                                                  outcome = "sexual_assault")) %>% 
  mutate(row_number = row_number(),
         column = glue::glue("Column\n({row_number})"))

main_table_graphic_weighted <- bind_rows(alc_main, alc_main_w, sex_main,sex_main_w) %>% 
  mutate(outcome = ifelse(outcome == "sexual_assault",
                          "Panel B: Sexual Assaults", "Panel A: Alcohol Offenses")) %>% 
  ggplot(aes(column, estimate, group = model,color = model,
             shape = model)) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.9),
                width = 0.4) +
  facet_wrap(~outcome,
             scales = "free_y") +
  ggthemes::scale_color_stata() +
  labs(x = "", y = "Point Estimate and 95% Confidence Interval",
       color = "", group = "",
       shape = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

