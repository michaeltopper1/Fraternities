library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)


if (!exists("semester_level")){
  semester_level <- read_csv("created_data/xmaster_data/semester_level.csv") 
}

if (!exists("semester_level_weekends")) {
  semester_level_weekends <- read_csv("created_data/xmaster_data/semester_level_weekends.csv") 
}

if (!exists("semester_level_weekdays")){
  semester_level_weekdays <- read_csv("created_data/xmaster_data/semester_level_weekdays.csv")
}

semester_level %>% 
  group_by(university_by_year_by_semester_number) %>% 
  count(lag_1, sort = T)
semester_level_weekends %>% 
  count(treatment, sort = T)
explanatory_vars <- c("lead_2", "lead_1", "treatment", "lag_1", "lag_2")

# fixed effects for semester_level ----------------------------------------
fixed_effects_s1 <- c("university", "semester_number")
fixed_effects_s2 <- c("university_by_semester_number", "semester_number")
fixed_effects_s3 <- c("university_by_year_by_semester_number")

semester_fixed_effects <- list(fixed_effects_s1, fixed_effects_s2)

# alcohol offenses --------------------------------------------------------


alc_s <- map(semester_fixed_effects, ~ifc::reghdfe(semester_level, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)
alc_weekends_s <- map(semester_fixed_effects, ~ifc::reghdfe(semester_level_weekends, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)
alc_weekdays_s <- map(semester_fixed_effects, ~ifc::reghdfe(semester_level_weekdays, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)

alc_table_sem <- ifc::main_table_semester(alc_s, alc_weekends_s, alc_weekdays_s)

alc_table_sem <- alc_table_sem %>% 
  kbl(booktabs = T,
      col.names = c(" ", "(1)", "(2)"),
      caption = "\\label{alc_semester}Effect of Moratoriums on Alcohol Offenses (Semesters)") %>% 
  kable_paper() %>% 
  pack_rows("Full Sample", 1, 11, bold = F, italic = T) %>% 
  pack_rows("Weekends", 12, 22, bold = F, italic = T) %>% 
  pack_rows("Weekdays", 23, 33, bold = F, italic = T) %>% 
  pack_rows("Fixed Effects",34, 36, bold = F, italic = T)
# drug offenses -----------------------------------------------------------


drug_s <- map(semester_fixed_effects, ~ifc::reghdfe(semester_level, c("drug_offense_per25"),explanatory_vars, ., "university")
)

drug_weekends_s <- map(semester_fixed_effects, ~ifc::reghdfe(semester_level_weekends, c("drug_offense_per25"),explanatory_vars, ., "university")
)

drug_weekdays_s <- map(semester_fixed_effects, ~ifc::reghdfe(semester_level_weekdays, c("drug_offense_per25"),explanatory_vars, ., "university")
)

drug_table_sem <- ifc::main_table_semester(drug_s, drug_weekends_s, drug_weekdays_s)

drug_table_sem <- drug_table_sem %>% 
  kbl(booktabs = T,
      col.names = c(" ", "(1)", "(2)"),
      caption = "\\label{drug_semester}Effect of Moratoriums on Drug Offenses (Semesters)") %>% 
  kable_paper() %>% 
  pack_rows("Full Sample", 1, 11, bold = F, italic = T) %>% 
  pack_rows("Weekends", 12, 22, bold = F, italic = T) %>% 
  pack_rows("Weekdays", 23, 33, bold = F, italic = T) %>% 
  pack_rows("Fixed Effects",34, 36, bold = F, italic = T)


# sexual assaults ---------------------------------------------------------

sex_s <- map(semester_fixed_effects, ~ifc::reghdfe(semester_level, c("sexual_assault_per25"),explanatory_vars, ., "university")
)

sex_weekends_s <- map(semester_fixed_effects, ~ifc::reghdfe(semester_level_weekends, c("drug_offense_per25"),explanatory_vars, ., "university")
)

sex_weekdays_s <- map(semester_fixed_effects, ~ifc::reghdfe(semester_level_weekdays, c("drug_offense_per25"),explanatory_vars, ., "university")
)

sex_table_sem <- ifc::main_table_semester(sex_s, sex_weekends_s, sex_weekdays_s)

sex_table_sem <- sex_table_sem %>% 
  kbl(booktabs = T,
      col.names = c(" ", "(1)", "(2)"),
      caption = "\\label{sex_semester}Effect of Moratoriums on Sexual Assaults (Semesters) ") %>% 
  kable_paper() %>% 
  pack_rows("Full Sample", 1, 11, bold = F, italic = T) %>% 
  pack_rows("Weekends", 12, 22, bold = F, italic = T) %>% 
  pack_rows("Weekdays", 23, 33, bold = F, italic = T) %>% 
  pack_rows("Fixed Effects",34, 36, bold = F, italic = T)
