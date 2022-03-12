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

party_school_table <- ifc::main_table(alc_party, last_panel = sex_party) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(party_school_data[[2]]$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(party_school_data[[3]]$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(party_school_data[[2]]$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(party_school_data[[3]]$sexual_assault_per25, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, col.names = c(" ", "All Schools", "Party Schools", "Non-Party Schools"),
      caption = "\\label{party_school_table}Effect of Moratoriums on Alcohol Offenses and Sexual Assault by Party School (OLS).") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = T, italic = F) %>% 
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B:", 9, 13, bold = T, italic = F) %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  add_header_above(c(" " = 1, "School Type" = 3)) %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as per-25000 enrolled students. The column ``All Schools'' represents specification (2) from the main results table. A party school classification is determined from niche.com's list of top partying schools. A university in the top 50 is considered a party school which amounts to 16 of the 38 universities. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since no university's academic calendar contains them. A moratorium is a temporary halt on fraternity-related activities with alcohol.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"),
           threeparttable = T) 
