library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}

laag <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + dplyr::lag(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}
leead <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + lead(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}
##541 highest


daily_crime_passing <- daily_crime %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(mstart_1 = ifelse(closure_1 == date,1 ,0),
         mstart_2 = ifelse(closure_2 == date, 1, 0),
         mstart_3 = ifelse(closure_3 == date, 1, 0)) %>% 
  mutate(mstart_1 = ifelse(university == "Florida International University" & date == "2018-01-04",
                           1, mstart_1)) %>% 
  mutate(across(c(mstart_1, mstart_2, mstart_3), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(weeks12 = laag(mstart_1, c(0:14)),
         weeks34 = laag(mstart_1, c(15:28)),
         weeks56 = laag(mstart_1, c(29:42)),
         weeks78 = laag(mstart_1, c(43:56)),
         weeks910 = laag(mstart_1, c(57:70))) %>% 
  mutate(weeks12_2 = laag(mstart_2, c(0:14)),
         weeks34_2 = laag(mstart_2, c(15:28)),
         weeks56_2 = laag(mstart_2, c(29:42)),
         weeks78_2 = laag(mstart_2, c(43:56)),
         weeks910_2 = laag(mstart_2, c(57:70))) %>% 
  mutate(weeks12_3 = laag(mstart_3, c(0:14)),
         weeks34_3 = laag(mstart_3, c(15:28)),
         weeks56_3 = laag(mstart_3, c(29:42)),
         weeks78_3 = laag(mstart_3, c(43:56)),
         weeks910_3 = laag(mstart_3, c(57:70))) %>% 
  mutate(across(starts_with("weeks"), ~ifelse(treatment == 1 & . == 1, ., 0))) %>% 
  mutate(weeks12_final = ifelse(weeks12 == 1 | weeks12_2 == 1 | weeks12_3 == 1, 1, 0),
         weeks34_final = ifelse(weeks34 == 1 | weeks34_2 == 1 | weeks34_3 == 1, 1, 0),
         weeks56_final = ifelse(weeks56 == 1 | weeks56_2 == 1 | weeks56_3 == 1, 1, 0),
         weeks78_final = ifelse(weeks78 == 1 | weeks78_2 == 1 | weeks78_3 == 1, 1, 0),
         weeks910_final = ifelse(weeks910 == 1 | weeks910_2 == 1 | weeks910_3 == 1, 1,0),
         weeks11plus_final = ifelse(treatment ==1 &   (weeks12_final !=1  &
                                                         weeks34_final != 1 &
                                                         weeks56_final != 1 &
                                                         weeks78_final != 1 &
                                                         weeks910_final != 1),1, 0)) %>% 
  mutate(weeks56_final = ifelse(university == "Louisiana State University and Agricultural & Mechanical College" &
                                  weeks12_final == 1 & weeks56_final == 1, 0, weeks56_final),
         weeks78_final = ifelse(university == "Louisiana State University and Agricultural & Mechanical College" &
                                  weeks34_final == 1 & weeks78_final == 1, 0, weeks78_final)) %>% 
  mutate(weeks78_final = ifelse(university == "Louisiana State University and Agricultural & Mechanical College" &
                                  weeks12_final == 1 & weeks78_final == 1, 0, weeks78_final),
         weeks910_final = ifelse(university == "Louisiana State University and Agricultural & Mechanical College" &
                                   weeks34_final == 1 & weeks910_final == 1, 0, weeks910_final),
         weeks910_final = ifelse(university == "Louisiana State University and Agricultural & Mechanical College" &
                                   weeks56_final == 1 & weeks910_final == 1, 0, weeks910_final)) 


fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")

explanatory_vars_1 <- c("weeks12_final")
explanatory_vars_2 <- c("weeks12_final", "weeks34_final")
explanatory_vars_3 <- c("weeks12_final", "weeks34_final", "weeks56_final")
explanatory_vars_4 <- c("weeks12_final", "weeks34_final", "weeks56_final", "weeks78_final")
explanatory_vars_5 <- c("weeks12_final", "weeks34_final", "weeks56_final", "weeks78_final", "weeks910_final")
explanatory_vars_6 <- c("weeks12_final", "weeks34_final", "weeks56_final", "weeks78_final", "weeks910_final", "weeks11plus_final")

ifc::reghdfe(daily_crime_passing, "alcohol_offense_per25", explanatory_vars_1,
             fixed_effects_preferred, "university")


# making function for subsetting by identifiable universities -------------
passing <- function(data, week_variable){
  unis <- data %>% 
    group_by(university) %>% 
    count({{week_variable}}) %>% 
    add_count() %>% 
    filter(nn == 1) %>% 
    pull(university)
  return(unis)
}
daily_crime_passing %>% 
  group_by(university) %>% 
  count(weeks12_final) %>% 
  add_count() %>% 
  filter(nn == 1)

# finding each subset -----------------------------------------------------

first_passing <- daily_crime_passing %>% 
  passing(weeks34_final)

second_passing <- daily_crime_passing %>% 
  passing(weeks56_final)

third_passing <- daily_crime_passing %>% 
  passing(weeks78_final) 

fourth_passing <- daily_crime_passing %>% 
  passing(weeks910_final)

fifth_passing <- daily_crime_passing %>% 
  passing(weeks11plus_final)

sixth_passing <- daily_crime_passing %>% 
  distinct(university) %>% pull()

identifying_universities <- map(list(first_passing,second_passing, third_passing, 
                                     fourth_passing, fifth_passing, sixth_passing), ~.x %>% length()) %>% 
  unlist()

# alcohol offenses regressions --------------------------------------------

passing_1 <- ifc::reghdfe(daily_crime_passing %>% 
                            filter(university %in% first_passing), "alcohol_offense_per25", explanatory_vars_1,
                          fixed_effects_preferred, "university")

passing_2 <- ifc::reghdfe(daily_crime_passing %>% 
                            filter(university %in% second_passing), "alcohol_offense_per25", explanatory_vars_2,
                          fixed_effects_preferred, "university")

passing_3 <- ifc::reghdfe(daily_crime_passing %>% 
                            filter(university %in% third_passing), "alcohol_offense_per25", explanatory_vars_3,
                          fixed_effects_preferred, "university")

passing_4 <- ifc::reghdfe(daily_crime_passing %>% 
                            filter(university %in% fourth_passing), "alcohol_offense_per25", explanatory_vars_4,
                          fixed_effects_preferred, "university")

passing_5 <- ifc::reghdfe(daily_crime_passing %>% 
                            filter(university %in% fifth_passing), "alcohol_offense_per25", explanatory_vars_5,
                          fixed_effects_preferred, "university")

passing_6 <- ifc::reghdfe(daily_crime_passing , "alcohol_offense_per25", explanatory_vars_6,
                          fixed_effects_preferred, "university")

alc_passing <- map_df(list(passing_1, passing_2, passing_3,
                           passing_4, passing_5, passing_6), ~ .x %>% broom::tidy(conf.int = T),
                      .id = "var") %>% 
  mutate(passing = case_when(
    var == 1 ~ "04 Schools",
    var == 2 ~ "11 Schools",
    var == 3 ~ "15 Schools",
    var == 4 ~ "22 Schools",
    var == 5 ~ "25 Schools",
    var == 6 ~ "37 Schools"
  ))



# sexual assault regressions ----------------------------------------------

passingsex_1 <- ifc::reghdfe(daily_crime_passing, "sexual_assault_per25", explanatory_vars_1,
                             fixed_effects_preferred, "university")

passingsex_2 <- ifc::reghdfe(daily_crime_passing %>% 
                               filter(university %in% second_passing), "sexual_assault_per25", explanatory_vars_2,
                             fixed_effects_preferred, "university")

passingsex_3 <- ifc::reghdfe(daily_crime_passing %>% 
                               filter(university %in% third_passing), "sexual_assault_per25", explanatory_vars_3,
                             fixed_effects_preferred, "university")

passingsex_4 <- ifc::reghdfe(daily_crime_passing %>% 
                               filter(university %in% fourth_passing), "sexual_assault_per25", explanatory_vars_4,
                             fixed_effects_preferred, "university")

passingsex_5 <- ifc::reghdfe(daily_crime_passing %>% 
                               filter(university %in% fifth_passing), "sexual_assault_per25", explanatory_vars_5,
                             fixed_effects_preferred, "university")

passingsex_6 <- ifc::reghdfe(daily_crime_passing %>% 
                               filter(university %in% sixth_passing), "sexual_assault_per25", explanatory_vars_6,
                             fixed_effects_preferred, "university")



alc_passing %>% 
  mutate(term = factor(term, levels = 
                         c("weeks12_final",
                           "weeks34_final",
                           "weeks56_final",
                           "weeks78_final",
                           "weeks910_final",
                           "weeks11plus_final"))) %>% 
  ggplot(aes(term, estimate)) +
  geom_point() +
  geom_line(aes(group = 1), linetype = "dashed", alpha = 0.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  geom_hline(yintercept = 0, color = "dark red") +
  scale_x_discrete(labels = c("Moratorium\nWeeks\n1 & 2",
                              "Moratorium\nWeeks\n3 & 4",
                              "Moratorium\nWeeks\n5 & 6",
                              "Moratorium\nWeeks\n7 & 8",
                              "Moratorium\nWeeks\n9 & 10",
                              "Moratorium\nWeeks\n11+")) +
  labs(x = "", y = "Point Estimate and 95% Confidence Interval",
       title = "Progression of Moratorium (Alcohol Offenses)") +
  facet_wrap(~passing, ncol = 1, strip.position = "left") +
  theme_minimal()
