library(tidyverse)
library(modelsummary)
library(fixest)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv")
}



explanatory_vars <- c("lead_2", "lead_1", "treatment", "lag_1", "lag_2")


# fixed effects for daily_level -------------------------------------------
fixed_effects_1 <- c("university", "date")
fixed_effects_2 <- c("university_by_semester_number", "semester_number")
fixed_effects_3 <- c("university_by_year_by_semester_number", "day_of_week")

daily_fixed_effects = list(fixed_effects_1, fixed_effects_2, fixed_effects_3)


alc_p <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)
alc_weekends_p <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime_weekends, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)
alc_weekdays_p <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime_weekdays, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)


alc_table_p <- ifc::main_table(alc_p, alc_weekends_p, alc_weekdays_p)

alc_table_p <- alc_table_p %>% 
  kbl(booktabs = T,
      col.names = c(" ", "(1)", "(2)", "(3)"),
      caption = "\\label{alc_p}Effect of Moratoriums on Alcohol Offenses (Poisson)") %>% 
  kable_paper() %>% 
  pack_rows("Full Sample", 1, 11, bold = F, italic = T) %>% 
  pack_rows("Weekends", 12, 22, bold = F, italic = T) %>% 
  pack_rows("Weekdays", 23, 33, bold = F, italic = T) %>% 
  pack_rows("Fixed Effects",34, 39, bold = F, italic = T)

drug_p <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime, c("drug_offense_per25"),explanatory_vars, ., "university")
)
drug_weekends_p <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime_weekends, c("drug_offense_per25"),explanatory_vars, ., "university")
)
drug_weekdays_p <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime_weekdays, c("drug_offense_per25"),explanatory_vars, ., "university")
)

drug_table_p <- ifc::main_table(drug_p, drug_weekends_p, drug_weekdays_p)

drug_table_p <- drug_table_p %>% 
  kbl(booktabs = T,
      col.names = c(" ", "(1)", "(2)", "(3)"),
      caption = "\\label{drug_p}Effect of Moratoriums on Drug Offenses (Poisson)") %>% 
  kable_paper() %>% 
  pack_rows("Full Sample", 1, 11, bold = F, italic = T) %>% 
  pack_rows("Weekends", 12, 22, bold = F, italic = T) %>% 
  pack_rows("Weekdays", 23, 33, bold = F, italic = T) %>% 
  pack_rows("Fixed Effects",34, 39, bold = F, italic = T)

sex_p <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime, c("sexual_assault_per25"),explanatory_vars, ., "university")
)
sex_weekends_p <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime_weekends, c("sexual_assault_per25"),explanatory_vars, ., "university")
)
sex_weekdays_p <- map(daily_fixed_effects, ~ifc::reghdfe_pois(daily_crime_weekdays, c("sexual_assault_per25"),explanatory_vars, ., "university")
)

sex_table_p <- ifc::main_table(sex_p, sex_weekends_p, sex_weekdays_p)

sex_table_p <- sex_table_p %>% 
  kbl(booktabs = T,
      col.names = c(" ", "(1)", "(2)", "(3)"),
      caption = "\\label{sex_p}Effect of Moratoriums on Sexual Assault Offenses (Poisson)") %>% 
  kable_paper() %>% 
  pack_rows("Full Sample", 1, 11, bold = F, italic = T) %>% 
  pack_rows("Weekends", 12, 22, bold = F, italic = T) %>% 
  pack_rows("Weekdays", 23, 33, bold = F, italic = T) %>% 
  pack_rows("Fixed Effects",34, 39, bold = F, italic = T)




