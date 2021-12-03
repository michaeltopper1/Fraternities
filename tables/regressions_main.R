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
fixed_effects_0 <- c("day_of_week", "academic_year", "spring_semester", "university", "holiday")
fixed_effects_1 <- c("day_of_week", "semester_by_academic_year", "university", "holiday")
fixed_effects_2 <- c("day_of_week", "university_by_academic_year_by_semester", "holiday")
fixed_effects_3 <- c("day_of_week_by_semester_by_academic_year", "university_by_academic_year_by_semester", "holiday")

daily_fixed_effects = list(fixed_effects_0,fixed_effects_1, fixed_effects_2, fixed_effects_3)


alc <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)

drug <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("drug_offense_per25"),explanatory_vars, ., "university")
)

sex <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("sexual_assault_per25"),explanatory_vars, ., "university")
)


main_table <- ifc::main_table(alc, drug, last_panel = sex) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$drug_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$drug_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$drug_offense_per25, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(daily_crime$drug_offense_per25, na.rm = T)),
          .before = 8) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          .before = 12) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)", "(4)"),
      digits = 3,
      caption = "\\label{main_table}Effect of Moratoriums on Alcohol Offenses, Drug Offenses, and Sexual Assault.") %>% 
  kable_paper() %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = F, italic = T) %>%
  pack_rows("Panel B: Drug Offenses", 5, 8, bold = F, italic = T) %>%
  pack_rows("Panel C: Sexual Assaults", 9, 12, bold = F, italic = T) %>% 
  pack_rows("Controls for Panels A-C:",13, 19, bold = F, italic = T ) %>% 
  footnote(list("Standard errors are clustered by university.",
                    "Offenses are per-25000 enrolled students.",
                    "Moratorium is a temporary halt on fraternity-related activities with alcohol.",
                    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

# table 2: weekends vs. full sample ---------------------------------------

fixed_effects_preferred <-  c("day_of_week_by_semester_by_academic_year", "university_by_academic_year_by_semester", "holiday")

data_subsets <- list(daily_crime, daily_crime_weekends, daily_crime_weekdays)

alc_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("alcohol_offense_per25"),explanatory_vars, fixed_effects_preferred, "university")
)

drug_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("drug_offense_per25"),explanatory_vars, fixed_effects_preferred, "university")
)

sex_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("sexual_assault_per25"),explanatory_vars, fixed_effects_preferred, "university")
)

weekend_table <- ifc::main_table(alc_weeksplit, drug_weeksplit, last_panel = sex_weeksplit) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$drug_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$drug_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$drug_offense_per25, na.rm = T)),
          .before = 8) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$sexual_assault_per25, na.rm = T)),
          .before = 12) %>% 
  kbl(booktabs = T, col.names = c(" ", "Full Sample", "Weekends", "Weekdays"),
      caption = "\\label{weekend_table}Effect of Moratoriums on Alcohol Offenses, Drug Offenses, and Sexual Assault by Weekend/Weekdays.") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = F, italic = T) %>% 
  pack_rows("Panel B: Drug Offenses", 5, 8, bold = F, italic = T) %>% 
  pack_rows("Panel C: Sexual Assaults", 9, 12, bold = F, italic = T) %>% 
  pack_rows("Controls for Panels A-C:", 13, 14, bold = F, italic = T) %>% 
  footnote(list("Standard errors are clustered by university.",
                    "Offenses are per-25000 enrolled students.",
                    "A moratorium is a temporary halt on fraternity-related activities with alcohol.",
                    "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001")) 

