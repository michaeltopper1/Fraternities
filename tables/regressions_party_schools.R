## Purpose of script: party schools are taken from niche.com top 50 party schools.
##
## Author: Michael Topper
##
## Date Last Edited: 2022-03-05
##

library(tidyverse)
library(kableExtra)
library(modelsummary)
library(fixest)

if (!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}

closures <- readxl::read_excel("data/closure_spreadsheet_final_2019.xlsx") %>% 
  janitor::clean_names() %>% 
  filter(university %in% ifc::moratorium_schools()) %>% 
  select(university, party_school)

daily_crime<- daily_crime %>% 
  left_join(closures, by = c("university")) 

party_school_data <- list(daily_crime, daily_crime %>% 
       filter(party_school == 1), daily_crime %>% 
       filter(party_school != 1))

explanatory_vars <- c("treatment")
fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")

alc_party<- map(party_school_data, ~ifc::reghdfe(., c("alcohol_offense_per25"),explanatory_vars, fixed_effects_preferred, "university")
)


sex_party <- map(party_school_data, ~ifc::reghdfe(., c("sexual_assault_per25"),explanatory_vars, fixed_effects_preferred, "university")
)

summarystats_no_treatment_party <- daily_crime %>% 
  filter(treatment == 0) %>% 
  group_by(party_school) %>% 
  summarize(across(.cols = c("alcohol_offense_per25","sexual_assault_per25"), 
                   ~mean(., na.rm = T), 
                   .names = "non_moratorium_avg_{.col}"))

summarystats_no_treatment <- daily_crime %>% 
  filter(treatment == 0) %>% 
  summarize(across(.cols = c("alcohol_offense_per25","sexual_assault_per25"), 
                   ~mean(., na.rm = T), 
                   .names = "non_moratorium_avg_{.col}"))


party_school_table <- ifc::main_table(alc_party, last_panel = sex_party) %>% 
  slice(1:6) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(party_school_data[[2]]$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(party_school_data[[3]]$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Non-Moratorium Mean", 
          `Model 1` = sprintf("%.3f",summarystats_no_treatment[[1]]),
          `Model 2` = sprintf("%.3f",summarystats_no_treatment_party[[2,2]]),
          `Model 3` = sprintf("%.3f",summarystats_no_treatment_party[[1,2]]),
          .before = 5) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(party_school_data[[2]]$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(party_school_data[[3]]$sexual_assault_per25, na.rm = T)),
          .before = 9) %>% 
  add_row(term = "Non-Moratorium Mean", 
          `Model 1` = sprintf("%.3f",summarystats_no_treatment[[2]]),
          `Model 2` = sprintf("%.3f",summarystats_no_treatment_party[[2,3]]),
          `Model 3` = sprintf("%.3f",summarystats_no_treatment_party[[1,3]]),
          .before = 10) %>% 
  kbl(booktabs = T, col.names = c(" ", "(1)", "(2)", "(3)"), align = "lccc",
      caption = "\\label{party_school_table}Effect of Moratoriums on Alcohol Offenses and Sexual Assault by Party School (OLS)") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 5, bold = F, italic = T) %>% 
  pack_rows("Panel B: Sexual Assaults", 6, 10, bold = F, italic = T, latex_gap_space = "0.5cm") %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11) %>% 
  add_header_above(c(" ", "All Schools" = 1, "Party Schools" = 1, "Non-Party Schools" = 1), line = F) %>% 
  add_header_above(c(" " = 1, "School Type" = 3)) %>% 
  row_spec(10, hline_after = F) %>% 
  column_spec(1, width = "8cm") %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as per-25000 enrolled students. The column All Schools represents the preferred specification (i.e., Column 2) from the main results table which includes day of the week, football game-day, semester number, and university-by-academic-year fixed effects. A party school classification is determined from Niche.com's list of top partying schools. A university in the top 50 is considered a party school which amounts to 16 of the 37 universities.",
                "* p < 0.1, ** p < 0.05, *** p < 0.01"),
           threeparttable = T) 


