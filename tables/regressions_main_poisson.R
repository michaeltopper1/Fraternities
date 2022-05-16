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


alc_p <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime, c("alcohol_offense"),explanatory_vars, ., "university")
)


sex_p <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime, c("sexual_assault"),explanatory_vars, ., "university")
)


# table 2: weekends vs. full sample ---------------------------------------

fixed_effects_preferred <-  c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")

data_subsets <- list(daily_crime, daily_crime_weekends, daily_crime_weekdays)

alc_weeksplit_p <- map(data_subsets, ~ifc::reghdfe_pois(., c("alcohol_offense"),explanatory_vars, fixed_effects_preferred, "university")
)


sex_weeksplit_p <- map(data_subsets, ~ifc::reghdfe_pois(., c("sexual_assault"),explanatory_vars, fixed_effects_preferred, "university")
)

alc_total_p <- c(alc_p, alc_weeksplit_p)[-4]
sex_total_p <- c(sex_p, sex_weeksplit_p)[-4]


main_table_p <- ifc::main_table(alc_total_p, last_panel = sex_total_p) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$alcohol_offense, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$alcohol_offense, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(daily_crime_weekends$alcohol_offense, na.rm = T)),
          `Model 5` = sprintf("%.3f",mean(daily_crime_weekdays$alcohol_offense, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$sexual_assault, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$sexual_assault, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(daily_crime_weekends$sexual_assault, na.rm = T)),
          `Model 5` = sprintf("%.3f",mean(daily_crime_weekdays$sexual_assault, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)", "(4)", "(5)"),
      digits = 3,
      caption = "\\label{main_table_p}Effect of Moratoriums on Alcohol Offenses and Sexual Assaults (Poisson)", align = 'lccccc') %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = F, italic = T) %>%
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = F, italic = T, latex_gap_space = "0.5cm") %>% 
  add_header_above(c(" " = 4, "Weekends" = 1, "Weekdays" = 1), line = F) %>% 
  add_header_above(c(" " = 4, "Specification (2)" = 2)) %>% 
  row_spec(c(8),hline_after=TRUE) %>% 
  # pack_rows("",11, 18, bold = F, italic = T, hline_before = T ) %>% 
  # row_spec(5, italic = T) %>% 
  column_spec(1, width = "8cm") %>% 
  row_spec(c(16), hline_after =T) %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as a count. Observation values may vary between specifications due to no variation with particular fixed effects. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since not in panel. A weekend is defined as Friday-Sunday while a weekday is defined as Monday-Thursday. A moratorium is a temporary halt on fraternity-related activities with alcohol.",
                "* p < 0.1, ** p < 0.05, *** p < 0.01"), threeparttable = T)


