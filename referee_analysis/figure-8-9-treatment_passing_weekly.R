library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)
library(patchwork)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}


# function for binned lags ------------------------------------------------

laag <- function(x, v){
  xx <- rep(0, length(x))
  for(i in v){
    xx <- xx + dplyr::lag(x, i)
  }
  xx[is.na(xx)] <- 0
  xx
}


# creating the passing treatment variables --------------------------------

daily_crime_passing <- daily_crime %>% 
  group_by(university) %>% 
  arrange(date) %>% 
  mutate(mstart_1 = ifelse(closure_1 == date,1 ,0),
         mstart_2 = ifelse(closure_2 == date, 1, 0),
         mstart_3 = ifelse(closure_3 == date, 1, 0)) %>% 
  mutate(mstart_1 = ifelse(university == "Florida International University" & date == "2018-01-04",
                           1, mstart_1)) %>% 
  mutate(across(c(mstart_1, mstart_2, mstart_3), ~ifelse(is.na(.), 0, .))) %>% 
  mutate(weeks1 = laag(mstart_1, c(0:7)),
         weeks2 = laag(mstart_1, c(8:14)),
         weeks3 = laag(mstart_1, c(15:21)),
         weeks4 = laag(mstart_1, c(22:28)),
         weeks5 = laag(mstart_1, c(29:35)),
         weeks6 = laag(mstart_1, c(36:42)),
         weeks7 = laag(mstart_1, c(43:49)),
         weeks8 = laag(mstart_1, c(50:56)),
         weeks9 = laag(mstart_1, c(57:63))) %>%
  mutate(weeks1_2 = laag(mstart_2, c(0:7)),
         weeks2_2 = laag(mstart_2, c(8:14)),
         weeks3_2 = laag(mstart_2, c(15:21)),
         weeks4_2 = laag(mstart_2, c(22:28)),
         weeks5_2 = laag(mstart_2, c(29:35)),
         weeks6_2 = laag(mstart_2, c(36:42)),
         weeks7_2 = laag(mstart_2, c(43:49)),
         weeks8_2 = laag(mstart_2, c(50:56)),
         weeks9_2 = laag(mstart_2, c(57:63))) %>% 
  mutate(weeks1_3 = laag(mstart_3, c(0:7)),
         weeks2_3 = laag(mstart_3, c(8:14)),
         weeks3_3 = laag(mstart_3, c(15:21)),
         weeks4_3 = laag(mstart_3, c(22:28)),
         weeks5_3 = laag(mstart_3, c(29:35)),
         weeks6_3 = laag(mstart_3, c(36:42)),
         weeks7_3 = laag(mstart_3, c(43:49)),
         weeks8_3 = laag(mstart_3, c(50:56)),
         weeks9_3 = laag(mstart_3, c(57:63))) %>% 
  mutate(across(starts_with("weeks"), ~ifelse(treatment == 1 & . == 1, ., 0))) %>% 
  mutate(weeks1_final = ifelse(weeks1 == 1 | weeks1_2 == 1 | weeks1_3 == 1, 1, 0),
         weeks2_final = ifelse(weeks2 == 1 | weeks2_2 == 1 | weeks2_3 == 1, 1, 0),
         weeks3_final = ifelse(weeks3 == 1 | weeks3_2 == 1 | weeks3_3 == 1, 1, 0),
         weeks4_final = ifelse(weeks4 == 1 | weeks4_2 == 1 | weeks4_3 == 1, 1, 0),
         weeks5_final = ifelse(weeks5 == 1 | weeks5_2 == 1 | weeks5_3 == 1, 1, 0),
         weeks6_final = ifelse(weeks6 == 1 | weeks6_2 == 1 | weeks6_3 == 1, 1, 0),
         weeks7_final = ifelse(weeks7 == 1 | weeks7_2 == 1 | weeks7_3 == 1, 1, 0),
         weeks8_final = ifelse(weeks8 == 1 | weeks8_2 == 1 | weeks8_3 == 1, 1, 0),
         weeks9_final = ifelse(weeks9 == 1 | weeks9_2 == 1 | weeks9_3 == 1, 1, 0),
         weeks9plus_final = ifelse(treatment ==1 &   (weeks1_final != 1  &
                                                        weeks2_final != 1 &
                                                        weeks3_final != 1 &
                                                        weeks4_final != 1 &
                                                        weeks5_final != 1 &
                                                        weeks6_final != 1 &
                                                        weeks7_final != 1 &
                                                        weeks8_final != 1 &
                                                        weeks9_final != 1), 1, 0))


# fixing louisiana universitiy because of awkward timings -----------------

daily_crime_passing <- daily_crime_passing %>% 
  mutate(weeks5_final = ifelse(university =="Louisiana State University and Agricultural & Mechanical College" &
                                 weeks1_final ==1 & 
                                 weeks5_final == 1, 0, weeks5_final),
         weeks6_final = ifelse(university =="Louisiana State University and Agricultural & Mechanical College" &
                                 weeks1_final ==1 & 
                                 weeks6_final == 1, 0, weeks6_final),
         weeks7_final = ifelse(university =="Louisiana State University and Agricultural & Mechanical College" &
                                 weeks2_final ==1 & 
                                 weeks7_final == 1, 0, weeks7_final),
         weeks8_final = ifelse(university =="Louisiana State University and Agricultural & Mechanical College" &
                                 weeks3_final ==1 & 
                                 weeks8_final == 1, 0, weeks8_final),
         weeks9_final = ifelse(university =="Louisiana State University and Agricultural & Mechanical College" &
                                 weeks4_final ==1 & 
                                 weeks9_final == 1, 0, weeks9_final))


# getting estimation variables --------------------------------------------

fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")

explanatory_vars <- c("weeks1_final", "weeks2_final", 
                        "weeks3_final", "weeks4_final",
                        "weeks5_final", "weeks6_final",
                        "weeks7_final", "weeks8_final",
                        "weeks9_final",
                        "weeks9plus_final")



# creating functions for list of universities and number ------------------

## gets list of universities for each count
passing <- function(data, week_variable){
  unis <- data %>% 
    group_by(university) %>% 
    count({{week_variable}}) %>% 
    filter({{week_variable}} == 1) %>% 
    pull(university)
  return(unis)
}

## gets counts for number of universities within each week
passing_count <- function(data, week_variable){
  week_variable = as.symbol(week_variable)
  unis <- data %>% 
    group_by(university) %>% 
    count({{week_variable}}) %>% 
    filter({{week_variable}} == 1) %>% 
    pull(university)
  return(unis)
}

## universities with over 2 months of moratoriums
two_month <- daily_crime_passing %>% 
  passing(weeks9plus_final)


# getting counts of universities within each week indicator ---------------

week_counts <- map(list("weeks1_final", "weeks2_final", "weeks3_final",
     "weeks4_final", "weeks5_final", "weeks6_final",
     "weeks7_final", "weeks8_final", "weeks9_final",
     "weeks9plus_final"), ~daily_crime_passing %>% 
  passing_count(.x) %>% length()) %>% 
  unlist()

week_counts_over_two_months <- map(list("weeks1_final", "weeks2_final", "weeks3_final",
                                        "weeks4_final", "weeks5_final", "weeks6_final",
                                        "weeks7_final", "weeks8_final", "weeks9_final",
                                        "weeks9plus_final"), ~daily_crime_passing %>% 
                                     filter(university %in% two_month) %>% 
                                     passing_count(.x) %>% length()) %>% 
  unlist()



# alcohol estimation and graph --------------------------------------------

alc_passing_1 <- ifc::reghdfe(daily_crime_passing , 
             "alcohol_offense_per25", explanatory_vars,
             fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(week = row_number(),
         university_count = week_counts) %>% 
  mutate(week_labels = glue::glue("Moratorium\nWeek {week}\n({university_count})")) %>% 
  mutate(week_labels = ifelse(row_number() ==10, glue::glue("Moratorium\nWeeks 10+\n({university_count})"),week_labels)) %>%
  mutate(model = "Panel B: Universities With Moratoriums Over Two Months")

alc_passing_graph <- ifc::reghdfe(daily_crime_passing %>% 
               filter(university %in% two_month), 
             "alcohol_offense_per25", explanatory_vars,
             fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(week = row_number(),
         university_count = week_counts_over_two_months) %>% 
  mutate(week_labels = glue::glue("Moratorium\nWeek {week}\n({university_count})")) %>%
  mutate(week_labels = ifelse(row_number() ==10, glue::glue("Moratorium\nWeeks 10+\n({university_count})"),week_labels)) %>%
  mutate(model = "Panel A: All Universities With Moratoriums") %>% 
  bind_rows(alc_passing_1) %>% 
  ggplot(aes(week_labels, estimate)) +
  geom_point() +
  geom_line(aes(group = 1), linetype = "dashed", alpha = 0.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  geom_hline(yintercept = 0, color = "black") +
  facet_wrap(~rev(model), ncol = 1, scales = "free_x") +
  labs(x = "", y = "Point Estimate and 95% Confidence Interval") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7))




# sexual assault estimation and graph -------------------------------------

sex_passing_1 <- ifc::reghdfe(daily_crime_passing , 
                              "sexual_assault_per25", explanatory_vars,
                              fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(week = row_number(),
         university_count = week_counts) %>% 
  mutate(week_labels = glue::glue("Moratorium\nWeek {week}\n({university_count})")) %>% 
  mutate(week_labels = ifelse(row_number() ==10, glue::glue("Moratorium\nWeeks 10+\n({university_count})"),week_labels)) %>%
  mutate(model = "Panel B: Universities With Moratoriums Over Two Months")


sex_passing_graph <- ifc::reghdfe(daily_crime_passing %>% 
                                    filter(university %in% two_month), 
                                  "sexual_assault_per25", explanatory_vars,
                                  fixed_effects_preferred, "university") %>% 
  broom::tidy(conf.int = T) %>% 
  mutate(week = row_number(),
         university_count = week_counts_over_two_months) %>% 
  mutate(week_labels = glue::glue("Moratorium\nWeek {week}\n({university_count})")) %>%
  mutate(week_labels = ifelse(row_number() ==10, glue::glue("Moratorium\nWeeks 10+\n({university_count})"),week_labels)) %>%
  mutate(model = "Panel A: All Universities With Moratoriums") %>% 
  bind_rows(sex_passing_1) %>% 
  ggplot(aes(week_labels, estimate)) +
  geom_point() +
  geom_line(aes(group = 1), linetype = "dashed", alpha = 0.5) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  geom_hline(yintercept = 0, color = "black") +
  facet_wrap(~rev(model), ncol = 1, scales = "free_x") +
  labs(x = "", y = "Point Estimate and 95% Confidence Interval") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7))

ggsave(filename = "figures/michael-topper-figure-8.pdf",plot = alc_passing_graph,
       width = 7.5, height = 4.5)

ggsave(filename = "figures/michael-topper-figure-9.pdf", plot = sex_passing_graph,
       width = 7.5, height = 4.5)
