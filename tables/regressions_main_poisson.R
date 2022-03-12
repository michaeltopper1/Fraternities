library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv")
}



explanatory_vars <- c("treatment")

daily_crime <- daily_crime %>% 
  group_by(university, date) %>% 
  mutate(university_by_date = cur_group_id()) %>% 
  ungroup()

# fixed effects for daily_level -------------------------------------------

fixed_effects_1 <- c("day_of_week", "academic_year", "spring_semester", "university", "holiday", "game_occurred")
fixed_effects_2 <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")
fixed_effects_3 <- c("day_of_week", "university_by_academic_year_by_semester", "holiday",  "spring_semester", "game_occurred")


daily_fixed_effects = list(fixed_effects_1, fixed_effects_2, fixed_effects_3)


alc <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime, c("alcohol_offense"),explanatory_vars, ., "university")
)


sex <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime, c("sexual_assault"),explanatory_vars, ., "university")
)


main_table_p <- ifc::main_table(alc, last_panel =sex) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$alcohol_offense, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$alcohol_offense, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$sexual_assault, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$sexual_assault, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)"),
      digits = 3,
      caption = "\\label{main_table_p}Effect of Moratoriums on Alcohol Offenses and Sexual Assault (Poisson Estimation).") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = T, italic = F) %>%
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B", 9,16, bold = T, itali = F) %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as a count. Observation values may vary between estimations due to no variation with particular fixed effects specifications. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since not in panel. A moratorium is a temporary halt on fraternity-related activities with alcohol. ",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"), threeparttable = T) 


# table 2: weekends vs. full sample ---------------------------------------

fixed_effects_preferred <-  c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")

data_subsets <- list(daily_crime, daily_crime_weekends, daily_crime_weekdays)

alc_weeksplit <- map(data_subsets, ~ifc::reghdfe_pois(., c("alcohol_offense"),explanatory_vars, fixed_effects_preferred, "university")
)


sex_weeksplit <- map(data_subsets, ~ifc::reghdfe_pois(., c("sexual_assault"),explanatory_vars, fixed_effects_preferred, "university")
)

weekend_table_p <- ifc::main_table(alc_weeksplit, last_panel = sex_weeksplit) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$sexual_assault_per25, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, col.names = c(" ", "All Days", "Weekends", "Weekdays"),
      caption = "\\label{weekend_table_p}Effect of Moratoriums on Alcohol Offenses and Sexual Assault by Weekend/Weekdays (Poisson Estimation).") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = T, italic = F) %>% 
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B:", 9, 13, bold = T, italic = F) %>% 
  add_header_above(c(" " = 1, "Days of the Week" = 3)) %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as a count. Observation values may vary between estimations due to no variation with particular fixed effects specifications. Weekends consist of Fridays, Saturdays, and Sundays. Weekdays consist of Monday through Thursday. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since not in panel. A moratorium is a temporary halt on fraternity-related activities with alcohol. ",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"), threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position")

