library(tidyverse)
library(lubridate)
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

## every single school
daily_crime_allschools<- read_csv("created_data/xmaster_data/daily_panel_allschools.csv") 

## never treated schools and moratorium schools put together.
daily_crime_nevertreated <- read_csv("created_data/xmaster_data/daily_panel_allschools.csv") %>% 
  filter((university %in% ifc::moratorium_schools()) | (university %in% ifc::never_treated_no_death()))

## never treated weekends
daily_crime_nevertreated_weekends <- daily_crime_nevertreated %>% 
  filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun")

## never treated weekends
daily_crime_nevertreated_weekdays <- daily_crime_nevertreated %>% 
  filter(!(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun"))


# creating the pseudo treatment schools ------------------------------------
laag <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + dplyr::lag(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}

death_dates <-read_csv("data/death_never_treated_dates.csv") %>% 
  mutate(death_date = mdy(death_date))

daily_crime_nevertreated_pseudo <- daily_crime_allschools %>% 
  filter(university %in% ifc::death_untreated_universities()) 


daily_crime_nevertreated_pseudo  <- daily_crime_nevertreated_pseudo %>% 
  left_join(death_dates, by = "university") 


# creating the pseudo treatment with 64 days ------------------------------

daily_crime_nevertreated_pseudo <- daily_crime_nevertreated_pseudo %>% 
  select(-treatment) %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  relocate(date, death_date) %>% 
  mutate(treatment = ifelse(date == death_date, 1, 0), .before = 1) %>% 
  mutate(treatment = laag(treatment, c(0:63))) %>% 
  ungroup()

daily_crime_nevertreated_pseudo_weekends <- daily_crime_nevertreated_pseudo %>% 
  filter(day_of_week == "Sat" | 
           day_of_week == "Sun" |
           day_of_week == "Fri")
daily_crime_nevertreated_pseudo_weekdays <- daily_crime_nevertreated_pseudo %>% 
  filter(!(day_of_week == "Sat" | 
             day_of_week == "Sun" |
             day_of_week == "Fri"))

# death moratorium and no-death schools -----------------------------------


daily_crime_deaths <- daily_crime_allschools %>% 
  filter(university %in% ifc::never_treated_no_death()) %>% 
  bind_rows(daily_crime_nevertreated_pseudo) 
  
daily_crime_deaths_weekends <- daily_crime_deaths %>% 
  filter((day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun"))

daily_crime_deaths_weekdays <- daily_crime_deaths %>% 
  filter(!(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun"))


# model preferences -------------------------------------------------------

fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")

explanatory_vars <- c("treatment")


subsamples_all <- list(daily_crime, daily_crime_weekends, daily_crime_weekdays,
                       daily_crime_nevertreated, daily_crime_nevertreated_weekends, daily_crime_nevertreated_weekdays,
                       daily_crime_nevertreated_pseudo, daily_crime_nevertreated_pseudo_weekends, daily_crime_nevertreated_pseudo_weekdays,
                       daily_crime_deaths, daily_crime_deaths_weekends, daily_crime_deaths_weekdays)

means <- map_df(subsamples_all, ~ .x  %>% summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T),
                                                   avg_sex = mean(sexual_assault_per25, na.rm = T)))

alc_estimates <- map_df(subsamples_all, ~ifc::reghdfe(., c("alcohol_offense_per25"),explanatory_vars, fixed_effects_preferred, "university") %>% 
         broom::tidy(conf.int = T), .id ="var" ) %>% 
  mutate(var = case_when(
    var == "1" |var == "2" | var == "3" ~ "Panel A:\n Main Sample",
    var == "4" | var == "5" | var == "6"~ "Panel B:\n Main Sample + Never Treated",
    var == "7" | var == "8" | var == "9" ~ "Panel C:\n Death Only Schools, 64-day Treatment",
    var == "10" | var == "11" | var == "12" ~ "Panel D:\n Death Only Schools, 64-day Treatment +\nNever Treated"
  )) %>% 
  mutate(day = rep(c("All Days", "Weekends", "Weekdays"), 4)) %>% 
  bind_cols(means)
alc_estimates <- alc_estimates %>% 
  mutate(across(starts_with("avg"), ~sprintf("%.3f", .))) %>% 
  mutate(alc_labels = glue::glue("{day}\nMean: {avg_alc}")) 

alc_labs <- alc_estimates %>% 
  pull(alc_labels) %>% rev()
alc_labs2 <- rev(sprintf("%.3f",alc_estimates$estimate))


alc_forest <- alc_estimates %>% 
  mutate(estimate = round(estimate, 2)) %>%
  mutate(estimate_label = sprintf("%.3f", estimate)) %>% 
  mutate(offense = rev(c(1:12))) %>% 
  mutate(var = factor(var, levels = c("Panel A:\n Main Sample", "Panel B:\n Main Sample + Never Treated", "Panel C:\n Death Only Schools, 64-day Treatment",
                                      "Panel D:\n Death Only Schools, 64-day Treatment +\nNever Treated"))) %>% 
  ggplot(aes(estimate, offense)) +
  geom_point(aes(shape = day), size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "dark red") +
  facet_wrap(~var, scales = "free_y") +
  scale_y_continuous(breaks = 1:length(alc_labs2),
                     labels = alc_labs,
                     sec.axis = sec_axis(~., breaks = 1:length(alc_labs2),
                                         labels = alc_labs2, 
                                         name = expression(paste("Estimate (", hat(beta), ")")))) +
  labs(y = "Sample", x = "", shape = "") +
  ggthemes::scale_color_stata() +
  theme_minimal() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_text(hjust = 1),
        axis.ticks.y = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1), 
        axis.text.y.left =element_text(size = 10),
        legend.position = "none",
        axis.line.x = element_line(color = "black"),
        panel.border = element_blank(),
        panel.background = element_rect(color = NA),
        strip.text = element_text(size = 12))


sex_estimates <- map_df(subsamples_all, ~ifc::reghdfe(., c("sexual_assault_per25"),explanatory_vars, fixed_effects_preferred, "university") %>% 
                          broom::tidy(conf.int = T), .id ="var" ) %>% 
  mutate(var = case_when(
    var == "1" |var == "2" | var == "3" ~ "Panel A:\n Main Sample",
    var == "4" | var == "5" | var == "6"~ "Panel B:\n Main Sample + Never Treated",
    var == "7" | var == "8" | var == "9" ~ "Panel C:\n Death Only Schools, 64-day Treatment",
    var == "10" | var == "11" | var == "12" ~ "Panel D:\n Death Only Schools, 64-day Treatment +\nNever Treated"
  )) %>% 
  mutate(day = rep(c("All Days", "Weekends", "Weekdays"), 4)) %>% 
  bind_cols(means)
sex_estimates <- sex_estimates %>% 
  mutate(across(starts_with("avg"), ~sprintf("%.3f", .))) %>% 
  mutate(sex_labels = glue::glue("{day}\nMean: {avg_sex}")) 

sex_labs <- sex_estimates %>% 
  pull(sex_labels) %>% rev()
sex_labs2 <- rev(sprintf("%.3f",sex_estimates$estimate))

sex_forest <- sex_estimates %>% 
  mutate(estimate = round(estimate, 2)) %>%
  mutate(estimate_label = sprintf("%.3f", estimate)) %>% 
  mutate(offense = rev(c(1:12))) %>% 
  mutate(var = factor(var, levels = c("Panel A:\n Main Sample", "Panel B:\n Main Sample + Never Treated",
                                      "Panel C:\n Death Only Schools, 64-day Treatment",
                                      "Panel D:\n Death Only Schools, 64-day Treatment +\nNever Treated"))) %>% 
  ggplot(aes(estimate, offense)) +
  geom_point(aes(shape = day), size = 3) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.1) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "dark red") +
  facet_wrap(~var, scales = "free_y") +
  scale_y_continuous(breaks = 1:length(sex_labs2),
                     labels = sex_labs,
                     sec.axis = sec_axis(~., breaks = 1:length(sex_labs2),
                                         labels = sex_labs2, 
                                         name = expression(paste("Estimate (", hat(beta), ")")))) +
  labs(y = "Sample", x = "", shape = "") +
  ggthemes::scale_color_stata() +
  theme_minimal() +
  theme(axis.line.y = element_blank(),
        axis.text.y = element_text(hjust = 1),
        axis.ticks.y = element_blank(),
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 1), 
        axis.text.y.left =element_text(size = 10),
        legend.position = "none",
        axis.line.x = element_line(color = "black"),
        panel.border = element_blank(),
        panel.background = element_rect(color = NA),
        strip.text = element_text(size = 12))

rm(subsamples_all)
rm(sex_pseudo, sex_pseudo_weekdays, sex_pseudo_weekends, daily_crime_deaths_weekdays, daily_crime_deaths_weekends,
   daily_crime_nevertreated_pseudo, daily_crime_nevertreated_pseudo_weekdays, daily_crime_nevertreated_pseudo_weekends,
   alc_pseudo, alc_pseudo_weekdays, alc_pseudo_weekends)
