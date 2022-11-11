library(tidyverse)
library(modelsummary)
library(fixest)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}


# defining lead and lag functions that can take vectors -------------------

leead <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + lead(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}
laag <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + lag(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}


## getting the beginning of the treatment so I know where to put the leads and lags
daily_crime_within <- daily_crime %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(closure_ind = ifelse(closure_1 == date | (closure_2 == date) | (closure_3 == date),1 ,0),
         closure_end_ind = ifelse(closure_1_end == date | closure_2_end == date | closure_3_end == date,
                                  1, 0)) %>% 
  mutate(closure_ind = ifelse(university == "Florida International University" & date == "2018-01-04",
                              1, closure_ind)) %>% 
  mutate(across(c(closure_ind, closure_end_ind), ~ifelse(is.na(.), 0, .)))


## getting mutually exclusive bins of the treatment variable
daily_crime_within <- daily_crime_within %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(within_1 = laag(closure_ind, c(0:32)),
         within_2 = laag(closure_ind, c(33:59)),
         within_3 = laag(closure_ind, c(60:541))) %>% 
  mutate(across(starts_with("within"), ~ifelse(treatment == 0 & . == 1, 0, .))) %>% 
  ungroup()
# filter(day_of_week == "Mon" | day_of_week == "Tue" | day_of_week == "Wed" | day_of_week == "Thu") %>%
# filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun")

explanatory_vars <- c("within_1",  "within_2", "within_3")
fe <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")

number_identified_by <- c()
for (i in 1:length(explanatory_vars)){
  number_identified_by[i] <- daily_crime_within %>% 
    filter(!!sym(explanatory_vars[i]) == 1) %>% 
    distinct(university) %>% 
    nrow()
}

universities_identified_by <- list()
for (i in 1:length(explanatory_vars)){
  universities_identified_by[i] <- daily_crime_within %>% 
    filter(!!sym(explanatory_vars[i]) == 1) %>% 
    distinct(university) 
}



## creating timeline of the mutually exclusive treatments
timeline <- c("Moratorium Days 0-32", "Moratorium Days 33-59", "Moratorium Days 60+")
timing <- tibble(number_identified_by, timeline, time = c(1:3))  %>% 
  mutate(timeline = glue::glue("{timeline} 
                               ({number_identified_by} Universities)"))

## pulling the timeline for ggplot labeling
timeline <- timing %>% 
  pull(-2)



# regressions -------------------------------------------------------------

daily_crime_within_identify_list <- map(universities_identified_by, ~daily_crime_within %>% 
      filter(university %in% .x))



alc_progression <- map_df(daily_crime_within_identify_list, 
                          ~ifc::reghdfe((.x), "alcohol_offense_per25", explanatory_vars, fe, "university") %>% 
                            broom::tidy(conf.int = T) %>% 
                            bind_cols(timing) %>% 
                            mutate(day_type = "Full Sample") %>% 
                            mutate(offense = "Alcohol Offense"),
                          .id ="var" )

main_alc_result <- ifc::reghdfe(daily_crime, "alcohol_offense_per25", "treatment", fe, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(var = "0",
         offense = "Alcohol Offense",
         day_type = "Full Sample") %>% 
  bind_cols(timing) %>% 
  mutate(timeline = "Entire Moratorium",
         time = 0)

alc_progression_graph <- alc_progression %>% 
  bind_rows(main_alc_result) %>%
  mutate(var = factor(var, levels = c("0","1", "2", "3"))) %>% 
  mutate(var = case_when(
    var == "0" ~ "Main Result",
    var == "1" ~ "37 ",
    var == "2" ~ "24 ",
    var == "3" ~"14 "
  )) %>% 
  mutate(timeline = fct_reorder(timeline, time)) %>% 
  mutate(var = fct_reorder(var, desc(var))) %>% 
  ggplot(aes(x = var, y = estimate)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
            data = ~ subset(., timeline %in% c("Entire Moratorium")), 
            fill = "light grey", inherit.aes = FALSE, alpha = 0.3) +
  facet_wrap(~timeline, ncol = 4, scales = "free_x") +
  geom_point(aes(shape = var), size = 2) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, color = "dark red") +
  annotate("segment",x=Inf,xend=-Inf,y=-Inf,yend=-Inf,color="black",lwd=1) +
  labs(x = " ", y = "Coefficient Estimate and 95% Confidence Interval",
       color = "Point Estimate Identified By:", shape = "Point Estimate Identified By:") +
  theme_minimal() +
  theme(legend.position = "none") 

rm(daily_crime_within)
# 
# alc_progression %>% 
#   mutate(var = factor(var, levels = c("1", "2", "3"))) %>% 
#   mutate(var = case_when(
#     var == "1" ~ "37\n Universities",
#     var == "2" ~ "24\n Universities",
#     var == "3" ~"14\n Universities"
#   )) %>% 
#   mutate(timeline = fct_reorder(timeline, time)) %>% 
#   mutate(var = fct_reorder(var, desc(var))) %>% 
#   ggplot(aes(x = var, y = estimate)) +
#   geom_point(aes(shape = var)) +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
#   geom_hline(yintercept = 0, color = "dark red") +
#   labs(x = " ", y = "Coefficient Estimate and 95% Confidence Interval",
#        color = "Point Estimate Identified By:", shape = "Point Estimate Identified By:") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 35),
#         legend.position = "bottom") 
# 
# 
# 
# 
# 
# sex_progression <- ifc::reghdfe(daily_crime_within, "sexual_assault_per25", explanatory_vars, fe, "university") %>% 
#   broom::tidy(conf.int = T) %>% 
#   bind_cols(timing) %>% 
#   mutate(day_type = "Full Sample") %>% 
#   mutate(offense = "Sexual Assault")
# 
# sex_progression_weekend <- ifc::reghdfe(daily_crime_within %>% 
#                                           filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun") ,
#                                         "sexual_assault_per25", explanatory_vars, fe, "university") %>% 
#   broom::tidy(conf.int = T) %>% 
#   bind_cols(timing) %>% 
#   mutate(day_type = "Weekends (Fri-Sun)")
# 
# 
# 
# # figures -----------------------------------------------------------------
# 
# universities_identified_by[[4]]
# cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# 
# treatment_passing <- sex_progression %>% 
#   bind_rows(sex_progression) %>% 
#   ggplot(aes(x = time, y = estimate, group = offense)) +
#   geom_path(aes(linetype = offense)) +
#   geom_point(aes(shape = offense)) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = offense), alpha =0.2) +
#   scale_x_continuous(breaks = c(1:7), labels = timeline) +
#   geom_hline(yintercept = 0, color = "dark red")  +
#   labs(x = "", y = "Coefficient Estimate and 95% Confidence Interval", group = " ", linetype = " ", shape = " ", fill = " ", color = " ") +
#   scale_fill_manual(values=cbbPalette) +
#   scale_color_manual(values = cbbPalette) +
#   theme_minimal() +
#   theme(legend.position = "bottom") +
#   facet_wrap(~offense, scales = "free", nrow = 3)
