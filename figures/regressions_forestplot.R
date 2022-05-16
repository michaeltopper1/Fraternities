library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)
library(patchwork)


if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv") 
}

daily_crime_allschools<- read_csv("created_data/xmaster_data/daily_panel_allschools.csv") 

daily_crime_nevertreated <- read_csv("created_data/xmaster_data/daily_panel_allschools.csv") %>% 
  filter((university %in% ifc::moratorium_schools()) | (university %in% ifc::never_treated_no_death()))

daily_crime_nevertreated_weekends <- daily_crime_nevertreated %>% 
  filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun")

## schools with moratorium and also schools with death and no moratorium
daily_crime_deaths <- daily_crime_allschools %>% 
  filter(university %in% ifc::death_universities()) 


fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")

explanatory_vars <- c("treatment")


subsamples_1 <- list(daily_crime, daily_crime_weekends)

subsamples_2 <- list( daily_crime_nevertreated, daily_crime_nevertreated_weekends,
                      daily_crime_deaths)

subsample_ols_alc <-  map_df(subsamples_1, ~ifc::reghdfe(., c("alcohol_offense_per25"),explanatory_vars, fixed_effects_preferred, "university") %>% 
                               broom::tidy(conf.int = T)) 
subsample_pois_alc <- map_df(subsamples_1, ~ifc::reghdfe_pois(., c("alcohol_offense"),explanatory_vars, fixed_effects_preferred, "university") %>% 
                               broom::tidy(conf.int = T))
subsample_2_ols_alc <- map_df(subsamples_2, ~ifc::reghdfe(., c("alcohol_offense_per25"),explanatory_vars, fixed_effects_preferred, "university") %>% 
                                broom::tidy(conf.int = T))

alc_estimates <- bind_rows(subsample_ols_alc, subsample_2_ols_alc)


subsample_ols_sex <-  map_df(subsamples_1, ~ifc::reghdfe(., c("sexual_assault_per25"),explanatory_vars, fixed_effects_preferred, "university") %>% 
                               broom::tidy(conf.int = T))
subsample_pois_sex <- map_df(subsamples_1, ~ifc::reghdfe_pois(., c("sexual_assault"),explanatory_vars, fixed_effects_preferred, "university") %>% 
                               broom::tidy(conf.int = T))
subsample_2_ols_sex <- map_df(subsamples_2, ~ifc::reghdfe(., c("sexual_assault_per25"),explanatory_vars, fixed_effects_preferred, "university") %>% 
                                broom::tidy(conf.int = T))


sex_estimates <- bind_rows(subsample_ols_sex, subsample_2_ols_sex)




model_data <- c("Main", "Main (Weekends)", "Never Treated","Never Treated (Weekends)", "Deaths Never Treated") %>% 
  as_tibble() %>% 
  rename(model = value)


alc_estimates <- alc_estimates %>% 
  bind_cols(model_data) %>% 
  mutate(offense = "alcohol_offense")

sex_estimates <- sex_estimates %>% 
  bind_cols(model_data) %>% 
  mutate(offense = "sexual_assault")





model_labs <- rev(c("Main Sample", "Main Sample \n(Weekends) ", "Main Sample +\nNever Treated  ","Main Sample +\nNever Treated (Weekends)", "Death Trigger Only +\nDeath Trigger No Moratorium"))
model_labs2 <- rev(sprintf("%.3f",alc_estimates$estimate))

alc_forest <- alc_estimates %>% 
  mutate(estimate = round(estimate, 3)) %>%
  mutate(estimate_label = sprintf("%.3f", estimate)) %>% 
  mutate(offense = rev(c(1:5))) %>% 
  ggplot(aes(estimate, offense)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "dark red") +
  scale_y_continuous(breaks = 1:length(model_labs),
                     labels = model_labs,
                     sec.axis = sec_axis(~., breaks = 1:length(model_labs),
                                         labels = model_labs2, 
                                         name = expression(paste("Estimate (", hat(beta), ")")))) +
  labs(y = "Sample", x = "") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_text(hjust = 1),
        axis.ticks.y = element_blank(),
        axis.text.y.right = element_text(hjust = -10, size = 10),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1), 
        axis.text.y.left =element_text(size = 10))

model_labs2_sex <- rev(sprintf("%.3f",sex_estimates$estimate))


sex_forest <- sex_estimates %>% 
  mutate(estimate = round(estimate, 3)) %>%
  mutate(estimate_label = sprintf("%.3f", estimate)) %>% 
  mutate(offense = rev(c(1:5))) %>% 
  ggplot(aes(estimate, offense)) +
  geom_point() +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "dark red") +
  scale_y_continuous(breaks = 1:length(model_labs),
                     labels = model_labs,
                     sec.axis = sec_axis(~., breaks = 1:length(model_labs),
                                         labels = model_labs2_sex, 
                                         name = expression(paste("Estimate (", hat(beta), ")")))) +
  labs(y = "Sample", x = " ") +
  theme_classic() +
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1),
        axis.text.y.left =element_text(size = 10),
        axis.text.y.right = element_text(size = 10))