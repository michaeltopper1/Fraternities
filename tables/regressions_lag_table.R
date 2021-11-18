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

offenses <- list("alcohol_offense_per25", "drug_offense_per25", "sexual_assault_per25")


full_sample <- map(offenses, ~ifc::reghdfe(daily_crime, .,explanatory_vars,fixed_effects_1 , "university")
)

weekends <- map(offenses, ~ifc::reghdfe(daily_crime_weekends, .,explanatory_vars, fixed_effects_1, "university")
)
weekdays <- map(offenses, ~ifc::reghdfe(daily_crime_weekdays, .,explanatory_vars, fixed_effects_1, "university")
)


lag_table <- ifc::main_table(full_sample, weekends, weekdays)

lag_table <- lag_table %>% 
  kbl(booktabs = T,
      col.names = c(" ", "Alcohol", "Drug", "Sexual Assault"),
      caption = "\\label{lag_table}Effect of Moratoriums on Alcohol Offenses (Daily-level)") %>% 
  kable_paper() %>% 
  pack_rows("Full Sample", 1, 11, bold = F, italic = T) %>% 
  pack_rows("Weekends", 12, 22, bold = F, italic = T) %>% 
  pack_rows("Weekdays", 23, 33, bold = F, italic = T) %>% 
  pack_rows("Fixed Effects", 34, 35, bold = F, italic = T) %>% 
  add_footnote(list("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
                    "Standard errors are clustered by university.",
                    "Offenses are per-25000 enrolled students.",
                    "Moratorium is a temporary hault on fraternity-related activities with alcohol.")) 




