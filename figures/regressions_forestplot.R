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

## getting lists of data sets to map over
subsamples_1 <- list(daily_crime, daily_crime_weekends)

subsamples_2 <- list( daily_crime_nevertreated, daily_crime_nevertreated_weekends,
                      daily_crime_deaths)

## for computing the means across all samples for labeling purposes
subsamples_all <- c(subsamples_1, subsamples_2)

means <- map_df(subsamples_all, ~ .x %>% summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T),
                                                  avg_sex = mean(sexual_assault_per25, na.rm = T)))



# estimating regressions --------------------------------------------------
subsample_ols_alc <-  map_df(subsamples_1, ~ifc::reghdfe(., c("alcohol_offense_per25"),explanatory_vars, fixed_effects_preferred, "university") %>% 
                               broom::tidy(conf.int = T)) 
subsample_2_ols_alc <- map_df(subsamples_2, ~ifc::reghdfe(., c("alcohol_offense_per25"),explanatory_vars, fixed_effects_preferred, "university") %>% 
                                broom::tidy(conf.int = T))

alc_estimates <- bind_rows(subsample_ols_alc, subsample_2_ols_alc) %>% 
  bind_cols(means)


subsample_ols_sex <-  map_df(subsamples_1, ~ifc::reghdfe(., c("sexual_assault_per25"),explanatory_vars, fixed_effects_preferred, "university") %>% 
                               broom::tidy(conf.int = T))
subsample_2_ols_sex <- map_df(subsamples_2, ~ifc::reghdfe(., c("sexual_assault_per25"),explanatory_vars, fixed_effects_preferred, "university") %>% 
                                broom::tidy(conf.int = T))


sex_estimates <- bind_rows(subsample_ols_sex, subsample_2_ols_sex) %>% 
  bind_cols(means)


# creating labels for the left y axis -------------------------------------

model_labs <- c("Main Sample", "Main Sample (Weekends) ", "Main Sample + Never Treated","Main Sample + Never Treated (Weekends)", "Death Trigger Only + Death Trigger No Moratorium")

alc_labs <- model_labs %>% 
  as_tibble() %>% 
  bind_cols(means) %>% 
  mutate(across(starts_with("avg"), ~sprintf("%.3f", .))) %>% 
  mutate(alc_labels = glue::glue("{value}\nMean of Dependent Variable: {avg_alc}"), .before = 1) %>% 
  pull(alc_labels) %>% 
  rev()

sex_labs <- model_labs %>% 
  as_tibble() %>% 
  bind_cols(means) %>% 
  mutate(across(starts_with("avg"), ~sprintf("%.3f", .))) %>% 
  mutate(sex_labels = glue::glue("{value}\nMean of Dependent Variable: {avg_sex}"), .before = 1) %>% 
  pull(sex_labels) %>% 
  rev()

## creates right y axis labels
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
                     labels = alc_labs,
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
                     labels = sex_labs,
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
