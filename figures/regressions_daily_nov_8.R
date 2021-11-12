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


alc <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)
alc_weekends <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime_weekends, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)
alc_weekdays <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime_weekdays, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)


alc_table <- ifc::main_table(alc, alc_weekends, alc_weekdays)

alc_table <- alc_table %>% 
  kbl(booktabs = T,
      col.names = c(" ", "(1)", "(2)", "(3)"),
      caption = "\\label{alc}Effect of Moratoriums on Alcohol Offenses (Daily-level)") %>% 
  kable_paper() %>% 
  pack_rows("Full Sample", 1, 11, bold = F, italic = T) %>% 
  pack_rows("Weekends", 12, 22, bold = F, italic = T) %>% 
  pack_rows("Weekdays", 23, 33, bold = F, italic = T) %>% 
  pack_rows("Fixed Effects",34, 39, bold = F, italic = T)

drug <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("drug_offense_per25"),explanatory_vars, ., "university")
)
drug_weekends <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime_weekends, c("drug_offense_per25"),explanatory_vars, ., "university")
)
drug_weekdays <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime_weekdays, c("drug_offense_per25"),explanatory_vars, ., "university")
)

drug_table <- ifc::main_table(drug, drug_weekends, drug_weekdays)

drug_table <- drug_table %>% 
  kbl(booktabs = T,
      col.names = c(" ", "(1)", "(2)", "(3)"),
      caption = "\\label{drug}Effect of Moratoriums on Drug Offenses (Daily-level)") %>% 
  kable_paper() %>% 
  pack_rows("Full Sample", 1, 11, bold = F, italic = T) %>% 
  pack_rows("Weekends", 12, 22, bold = F, italic = T) %>% 
  pack_rows("Weekdays", 23, 33, bold = F, italic = T) %>% 
  pack_rows("Fixed Effects",34, 39, bold = F, italic = T)

sex <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("sexual_assault_per25"),explanatory_vars, ., "university")
)
sex_weekends <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime_weekends, c("sexual_assault_per25"),explanatory_vars, ., "university")
)
sex_weekdays <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime_weekdays, c("sexual_assault_per25"),explanatory_vars, ., "university")
)

sex_table <- ifc::main_table(sex, sex_weekends, sex_weekdays)

sex_table <- sex_table %>% 
  kbl(booktabs = T,
      col.names = c(" ", "(1)", "(2)", "(3)"),
      caption = "\\label{sex}Effect of Moratoriums on Sexual Assault Offenses (Daily-level)") %>% 
  kable_paper() %>% 
  pack_rows("Full Sample", 1, 11, bold = F, italic = T) %>% 
  pack_rows("Weekends", 12, 22, bold = F, italic = T) %>% 
  pack_rows("Weekdays", 23, 33, bold = F, italic = T) %>% 
  pack_rows("Fixed Effects",34, 39, bold = F, italic = T)




