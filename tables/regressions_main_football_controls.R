## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-21
##

library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv") %>% 
    mutate(home_game = ifelse(is.na(home_game), 0, home_game)) 
}

if (!exists("daily_crime_all")) {
  daily_crime_all <- read_csv("created_data/xmaster_data/daily_panel_allschools.csv") %>% 
    mutate(home_game = ifelse(is.na(home_game), 0, home_game)) 
}

if (!exists("daily_crime_weekends")) {
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel.csv") %>% 
    mutate(home_game = ifelse(is.na(home_game), 0, home_game)) %>% 
    filter(day_of_week == "Sat" | day_of_week == "Sun" | day_of_week == "Fri")
}
if (!exists("daily_crime_weekdays")) {
  daily_crime_weekdays<- read_csv("created_data/xmaster_data/daily_panel.csv") %>% 
    mutate(home_game = ifelse(is.na(home_game), 0, home_game)) %>% 
    filter(!(day_of_week == "Sat" | day_of_week == "Sun" | day_of_week == "Fri"))
}

fixed_effects_1 <- c("day_of_week", "academic_year", "spring_semester", "university", "holiday")
fixed_effects_2 <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester")
fixed_effects_3 <- c("day_of_week", "university_by_academic_year_by_semester", "holiday",  "spring_semester")

fixed_effects_1n <- c("day_of_week", "academic_year", "spring_semester", "university", "holiday", "game_occurred")
fixed_effects_2n <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")
fixed_effects_3n <- c("day_of_week", "university_by_academic_year_by_semester", "holiday",  "spring_semester", "game_occurred")

daily_fixed_effects = list(fixed_effects_1, fixed_effects_2, fixed_effects_3, fixed_effects_1n, fixed_effects_2n, fixed_effects_3n)

explanatory_vars <- c("treatment")

alc <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)

sex <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("sexual_assault_per25"),explanatory_vars, ., "university")
)


main_table_adjusted <- ifc::main_table(alc, last_panel = sex) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 5` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 6` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 5` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 6` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)", "(1)", 
                    "(2)", "(3)"),
      digits = 3,
      caption = "\\label{main_table_adjusted}Effect of Moratoriums on Alcohol Offenses and Sexual Assaults (OLS).") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  add_header_above(c(" " = 1, "Without Football" = 3, "With Football Controls" = 3)) %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = T, italic = F) %>%
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B:",9, 16, bold = T, italic = F ) %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as per-25000 enrolled students. Weekends consist of Fridays, Saturdays, and Sundays. Weekdays consist of Monday through Thursday. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since these holiday's are not on any university's academic calendar. A moratorium is a temporary halt on fraternity-related activities with alcohol. Specification (2) is the preferred specification due to the flexibility of the fixed effects and the conservativeness of the estimates.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"), threeparttable = T)

fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester")

main_specifications <- list(fixed_effects_1, fixed_effects_2, fixed_effects_3)
interaction <- c("treatment*game_occurred")

datas <- list(daily_crime, daily_crime_weekends, daily_crime_weekdays)
alc_interaction <- map(datas, ~ifc::reghdfe(., "alcohol_offense_per25", interaction, fixed_effects_preferred, "university")) 
sex_interaction <- map(datas, ~ifc::reghdfe(., "sexual_assault_per25", interaction, fixed_effects_preferred, "university")) 


effect_game_day_interaction <- ifc::main_table(alc_interaction, last_panel = sex_interaction) %>% 
  kbl(booktabs = T, col.names = c(" ", "All Days", "Weekends", "Weekdays"),
      caption = "\\label{effect_game_day_interaction}Effect of Moratoriums on Alcohol Offenses and Sexual Assault by Weekend/Weekdays (OLS).") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 7, bold = T, italic = F) %>% 
  pack_rows("Panel B: Sexual Assaults", 8, 14, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B:", 15, 18, bold = T, italic = F) %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  add_header_above(c(" " = 1, "Days of the Week" = 3)) %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as per-25000 enrolled students. The column `All Days' represents specification (2) from the main results table. Weekends consist of Fridays, Saturdays, and Sundays. Weekdays consist of Monday through Thursday. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since no university's academic calendar contains them. A moratorium is a temporary halt on fraternity-related activities with alcohol.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"),
           threeparttable = T) 

effect_game_moratorium_alc <- map(datas, ~ifc::reghdfe(., "alcohol_offense_per25", "treatment:game_occurred", fixed_effects_preferred, "university")) 
effect_game_moratorium_sex <- map(datas, ~ifc::reghdfe(., "sexual_assault_per25", "treatment:game_occurred", fixed_effects_preferred, "university")) 

effect_game_day_moratorium_table <- ifc::main_table(effect_game_moratorium_alc, last_panel = effect_game_moratorium_sex) %>% 
  kbl(booktabs = T, col.names = c(" ", "All Days", "Weekends", "Weekdays"),
      caption = "\\label{effect_game_day_moratorium_table}Effect of Moratoriums on Alcohol Offenses and Sexual Assault by Weekend/Weekdays (OLS).") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 3, bold = T, italic = F) %>% 
  pack_rows("Panel B: Sexual Assaults", 4, 6, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B:", 7, 10, bold = T, italic = F) %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  add_header_above(c(" " = 1, "Days of the Week" = 3)) %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as per-25000 enrolled students. The column `All Days' represents specification (2) from the main results table. Weekends consist of Fridays, Saturdays, and Sundays. Weekdays consist of Monday through Thursday. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since no university's academic calendar contains them. A moratorium is a temporary halt on fraternity-related activities with alcohol.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"),
           threeparttable = T) 

