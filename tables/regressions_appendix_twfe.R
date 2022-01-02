library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)

if (!exists("daily_crime")){
  daily_crime <- read_csv(paste0(here::here("created_data/xmaster_data/daily_panel.csv")))
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv(paste0(here::here("created_data/xmaster_data/daily_panel_weekends.csv")))
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv(paste0(here::here("created_data/xmaster_data/daily_panel_weekdays.csv")))
}



explanatory_vars <- c("treatment")


# table 2: weekends vs. full sample ---------------------------------------

fixed_effects_preferred <- c("university", "date")
# fixed_effects_preferred_l <-  c("day_of_week", "university_by_academic_year_by_semester", "holiday")
data_subsets <- list(daily_crime, daily_crime_weekends, daily_crime_weekdays)

alc_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("alcohol_offense_per25"),explanatory_vars, fixed_effects_preferred, "university")
)

drug_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("drug_offense_per25"),explanatory_vars, fixed_effects_preferred, "university")
)

sex_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("sexual_assault_per25"),explanatory_vars, fixed_effects_preferred, "university")
)

twfe_table <- ifc::main_table(alc_weeksplit, drug_weeksplit, last_panel = sex_weeksplit) %>% 
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
  kbl(booktabs = T, col.names = c(" ", "All Days", "Weekends", "Weekdays"),
      caption = "\\label{twfe_table}Effect of Moratoriums on Alcohol Offenses, Drug Offenses, and Sexual Assault by Weekend/Weekdays  (No Negative Weights-OLS).") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = T, italic = F) %>% 
  pack_rows("Panel B: Drug Offenses", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Panel C: Sexual Assaults", 9, 12, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-C:", 13, 14, bold = T, italic = F) %>% 
  add_header_above(c(" " = 1, "Days of the Week" = 3)) %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as per-25000 enrolled students. The column `All Days' represents specification (3) from the main results table. Weekends consist of Fridays, Saturdays, and Sundays. Weekdays consist of Monday through Thursday. A moratorium is a temporary halt on fraternity-related activities with alcohol. The specification used in this table has no negative weights and thus, sign reversal is impossible.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"),
           threeparttable = T) 