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


# fixed effects for daily_level -------------------------------------------
fixed_effects_1 <- c("day_of_week", "academic_year", "spring_semester", "university", "holiday")
fixed_effects_2 <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester")
fixed_effects_3 <- c("day_of_week", "university_by_academic_year_by_semester", "holiday",  "spring_semester")


daily_fixed_effects = list(fixed_effects_1, fixed_effects_2, fixed_effects_3)

alc <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)


sex <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("sexual_assault_per25"),explanatory_vars, ., "university")
)




library(gt)
main_table_alc <- ifc::main_table(last_panel = alc) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  gt() %>% 
  cols_label(term = " ", `Model 1` = "(1)", `Model 2` = "(2)", `Model 3` = "(3)") %>% 
  tab_header(title = "Effect of Moratoriums on Alcohol Offenses (OLS)") %>% 
  tab_options(
    table.width = pct(100)
  ) %>% 
  tab_source_note(list("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

main_table_sex <- ifc::main_table(last_panel = sex) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          .before = 4) %>% 
  gt() %>% 
  cols_label(term = " ", `Model 1` = "(1)", `Model 2` = "(2)", `Model 3` = "(3)") %>% 
  tab_header(title = "Effect of Moratoriums on Sexual Assaults (OLS)") %>% 
  tab_options(
    table.width = pct(100)
  ) %>% 
  tab_source_note(list("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))


# table 2: weekends vs. full sample ---------------------------------------

fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester")
# fixed_effects_preferred_l <-  c("day_of_week", "university_by_academic_year_by_semester", "holiday")
data_subsets <- list(daily_crime, daily_crime_weekends, daily_crime_weekdays)

alc_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("alcohol_offense_per25"),explanatory_vars, fixed_effects_preferred, "university")
)


sex_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("sexual_assault_per25"),explanatory_vars, fixed_effects_preferred, "university")
)

weekend_table_alc <- ifc::main_table(last_panel = alc_weeksplit) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  gt() %>% 
  tab_header(title = "Effect of Moratoriums on Alcohol Offenses by Weekend/Weekdays (OLS)") %>% 
  cols_label(term = " ", `Model 1` = "All Days", `Model 2` = "Weekends", `Model 3` = "Weekdays") %>% 
  tab_spanner(label = "Days of the Week" , columns =c(2:4)) %>% 
  tab_source_note(list("Standard errors are clustered by university and each offense is defined as per-25000 enrolled students. The column `All Days' represents specification (2) from the main results table. Weekends consist of Fridays, Saturdays, and Sundays. Weekdays consist of Monday through Thursday. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since no university's academic calendar contains them. A moratorium is a temporary halt on fraternity-related activities with alcohol.",
                       "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001")) %>% 
  tab_options(
    table.width = pct(100)
  )
  

weekend_table_sex <- ifc::main_table(last_panel = sex_weeksplit) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$sexual_assault_per25, na.rm = T)),
          .before = 4) %>% 
  gt() %>% 
  tab_header(title = "Effect of Moratoriums on Sexual Assaults by Weekend/Weekdays (OLS)") %>% 
  cols_label(term = " ", `Model 1` = "All Days", `Model 2` = "Weekends", `Model 3` = "Weekdays") %>% 
  tab_spanner(label = "Days of the Week" , columns =c(2:4)) %>% 
  tab_source_note(list("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001")) %>% 
  tab_options(
    table.width = pct(100)
  )
