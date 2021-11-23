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
fixed_effects_0 <- c()
fixed_effects_1 <- c("university_by_semester_number", "semester_number", "day_of_week")
fixed_effects_2 <- c("university", "date")
fixed_effects_3 <- c("university_by_year_by_semester_number", "day_of_week")


daily_fixed_effects = list(fixed_effects_0,fixed_effects_1, fixed_effects_2, fixed_effects_3)


alc <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime, c("alcohol_offense"),explanatory_vars, ., "university")
)

drug <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime, c("drug_offense"),explanatory_vars, ., "university")
)

sex <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime, c("sexual_assault"),explanatory_vars, ., "university")
)


main_table_p <- ifc::table_panels(alc, drug, sex) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$alcohol_offense, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$alcohol_offense, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(daily_crime$alcohol_offense, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$drug_offense, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$drug_offense, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$drug_offense, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(daily_crime$drug_offense, na.rm = T)),
          .before = 14) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$sexual_assault, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$sexual_assault, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(daily_crime$sexual_assault, na.rm = T)),
          .before = 24) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)", "(4)"),
      digits = 3,
      caption = "\\label{main_table_p}Effect of Moratoriums on Alcohol Offenses, Drug Offenses, and Sexual Assault (Poisson Regressions).") %>% 
  kable_paper() %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 10, bold = F, italic = T) %>% 
  pack_rows("Panel B: Drug Offenses", 11, 20, bold = F, italic = T) %>% 
  pack_rows("Panel C: Sexual Assaults", 21, 30, bold = F, italic = T) %>% 
  add_footnote(list("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
                    "Standard errors are clustered by university.",
                    "Offenses are counts.",
                    "Moratorium is a temporary hault on fraternity-related activities with alcohol.")) 

# table 2: weekends vs. full sample ---------------------------------------

fixed_effects_preferred <- c("university", "date")

data_subsets <- list(daily_crime, daily_crime_weekends, daily_crime_weekdays)

alc_weeksplit <- map(data_subsets, ~ifc::reghdfe_pois(., c("alcohol_offense"),explanatory_vars, fixed_effects_preferred, "university")
)

drug_weeksplit <- map(data_subsets, ~ifc::reghdfe_pois(., c("drug_offense"),explanatory_vars, fixed_effects_preferred, "university")
)

sex_weeksplit <- map(data_subsets, ~ifc::reghdfe_pois(., c("sexual_assault"),explanatory_vars, fixed_effects_preferred, "university")
)

weekend_table_p <- ifc::table_panels(alc_weeksplit, drug_weeksplit, sex_weeksplit) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$alcohol_offense, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$alcohol_offense, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$drug_offense, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$drug_offense, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$drug_offense, na.rm = T)),
          .before = 10) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$sexual_assault, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$sexual_assault, na.rm = T)),
          .before = 16) %>% 
  kbl(booktabs = T, col.names = c(" ", "Full Sample", "Weekends", "Weekdays"),
      caption = "\\label{weekend_table_p}Effect of Moratoriums on Alcohol Offenses, Drug Offenses, and Sexual Assault by Weekend/Weekdays (Poisson Regressions).") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 6, bold = F, italic = T) %>% 
  pack_rows("Panel B: Drug Offenses", 7, 12, bold = F, italic = T) %>% 
  pack_rows("Panel C: Sexual Assaults", 13, 18, bold = F, italic = T) %>% 
  add_footnote(list("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
                    "Standard errors are clustered by university.",
                    "Offenses are counts.",
                    "Moratorium is a temporary hault on fraternity-related activities with alcohol.")) 

