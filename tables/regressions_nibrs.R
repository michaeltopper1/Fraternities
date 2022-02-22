## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-18
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

if (!exists("nibrs")) {
  nibrs <- read_csv("created_data/xmaster_data/nibrs_final.csv")
}

if (!exists("nibrs_treated")) {
  nibrs_treated <- nibrs %>% 
    filter(university %in% ifc::moratorium_schools())
}

if (!exists("nibrs_treated_weekends")) {
  nibrs_treated_weekends <- nibrs %>% 
    filter(university %in% ifc::moratorium_schools()) %>% 
    filter(day_of_week == "Sun" | day_of_week == "Fri" | day_of_week == "Sat")
}


if (!exists("nibrs_treated_nonschool")) {
  nibrs_treated_nonschool <- nibrs %>% 
    filter(university %in% ifc::moratorium_schools()) %>% 
    filter(ori_type == "nonschool")
}

explanatory_vars <- c("treatment")

# fixed effects for daily_level -------------------------------------------
fixed_effects_1 <- c("day_of_week", "academic_year", "spring_semester", "ori", "holiday", "game_occurred")
fixed_effects_2 <- c("day_of_week", "ori_by_academic_year", "holiday", "spring_semester", "game_occurred")
fixed_effects_3 <- c("day_of_week", "ori_by_academic_year_by_semester", "holiday",  "spring_semester", "game_occurred")

daily_fixed_effects = list(fixed_effects_1, fixed_effects_2, fixed_effects_3)


alc_nibrs <- map(daily_fixed_effects, ~ifc::reghdfe(nibrs_treated, c("alcohol_arrest_total_per25"), explanatory_vars, ., "ori")
)

sex_nibrs <- map(daily_fixed_effects, ~ifc::reghdfe(nibrs_treated, c("sexual_assault_per25"), explanatory_vars, ., "ori")
)


alc_nibrs_college <-  map(daily_fixed_effects, ~ifc::reghdfe(nibrs_treated, c("alcohol_arrest_college_aged_per25"), explanatory_vars, ., "ori")
)

sex_nibrs_college <- map(daily_fixed_effects, ~ifc::reghdfe(nibrs_treated, c("college_age_sexual_assault_per25"), explanatory_vars, ., "ori")
)




## amounts to only 9 agencies that are non-school police
alc_nibrs_nonschool <- map(daily_fixed_effects, ~ifc::reghdfe(nibrs_treated %>% 
                                                                filter(ori_type == "nonschool"), c("alcohol_arrest_total_per25"), explanatory_vars, ., "ori")
)

sex_nibrs_nonschool <- map(daily_fixed_effects, ~ifc::reghdfe(nibrs_treated %>% 
                                                                filter(ori_type == "nonschool"), c("sexual_assault_per25"), explanatory_vars, ., "ori")
)


alc_nibrs_nonschool_college <- map(daily_fixed_effects, ~ifc::reghdfe(nibrs_treated %>% 
                                                                filter(ori_type == "nonschool"), c("alcohol_arrest_college_aged_per25"), explanatory_vars, ., "ori")
)

sex_nibrs_nonschool_college <- map(daily_fixed_effects, ~ifc::reghdfe(nibrs_treated %>% 
                                                                filter(ori_type == "nonschool"), c("college_age_sexual_assault_per25"), explanatory_vars, ., "ori")
)








# main results with nibrs -------------------------------------------------

nibrs_table <- ifc::main_table(alc_nibrs, last_panel = sex_nibrs) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_total_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_total_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_total_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated_nonschool$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(nibrs_treated_nonschool$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(nibrs_treated_nonschool$sexual_assault_per25, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)"),
      digits = 3,
      caption = "\\label{nibrs_table}Effect of Moratoriums on Alcohol Arrests and Sexual Assaults in Universities and Nearby Agencies (OLS).") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = T, italic = F) %>%
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B:",9, 16, bold = T, italic = F ) %>% 
  footnote(list("14/38 univerisites included due to reporting issues. Standard errors clustered by agency. Alcohol offenses are only arrests. Sexual assaults are the sum of rape, fondling, sexual assault with an object, and statutory rape. Offenses are per-25000 enrolled students.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"), threeparttable = T)


nibrs_table_college <- ifc::main_table(alc_nibrs_college, last_panel = sex_nibrs_college) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_college_aged_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_college_aged_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_college_aged_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated_nonschool$college_age_sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(nibrs_treated_nonschool$college_age_sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(nibrs_treated_nonschool$college_age_sexual_assault_per25, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)"),
      digits = 3,
      caption = "\\label{main_table}Effect of Moratoriums on Alcohol Arrests and Sexual Assaults in Universities and Nearby Agencies College Aged Only (OLS).") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = T, italic = F) %>%
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B:",9, 16, bold = T, italic = F ) %>% 
  footnote(list("14/38 univerisites included due to reporting issues. Standard errors clustered by agency. Alcohol offenses are only arrests. Sexual assaults are the sum of rape, fondling, sexual assault with an object, and statutory rape. Offenses are per-25000 enrolled students.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"), threeparttable = T)




# spillovers --------------------------------------------------------------


spillovers <- ifc::main_table(alc_nibrs_nonschool, last_panel = sex_nibrs_nonschool) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_total_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_total_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_total_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated_nonschool$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(nibrs_treated_nonschool$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(nibrs_treated_nonschool$sexual_assault_per25, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)"),
      digits = 3,
      caption = "\\label{spillovers}Effect of Moratoriums on Alcohol Arrests and Sexual Assaults in Nearby Agencies (OLS).") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = T, italic = F) %>%
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B:",9, 16, bold = T, italic = F ) %>% 
  footnote(list("Nine nearby agencies are included. Standard errors clustered by agency. Alcohol offenses are only arrests. Sexual assaults are the sum of rape, fondling, sexual assault with an object, and statutory rape. Offenses are per-25000 enrolled students.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"), threeparttable = T)

spillovers_college_aged <- ifc::main_table(alc_nibrs_nonschool_college, last_panel = sex_nibrs_nonschool_college) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_college_aged_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_college_aged_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_college_aged_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated_nonschool$college_age_sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(nibrs_treated_nonschool$college_age_sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(nibrs_treated_nonschool$college_age_sexual_assault_per25, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)"),
      digits = 3,
      caption = "\\label{main_table}Effect of Moratoriums on Alcohol Arrests and Sexual Assaults for College-Aged Individuals in Nearby Agencies (OLS).") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = T, italic = F) %>%
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B:",9, 16, bold = T, italic = F ) %>% 
  footnote(list("Nine nearby agencies are included. Standard errors clustered by agency. Alcohol offenses are only arrests. Sexual assaults are the sum of rape, fondling, sexual assault with an object, and statutory rape. Offenses are per-25000 enrolled students.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"), threeparttable = T)



alc_nibrs_nonschool_college_weekends <- map(daily_fixed_effects, ~ifc::reghdfe(nibrs_treated_weekends %>% 
                                         filter(ori_type == "nonschool"), c("alcohol_arrest_total_per25"), explanatory_vars, ., "ori")
)
sex_nibrs_nonschool_college_weekends <- map(daily_fixed_effects, ~ifc::reghdfe(nibrs_treated %>% 
                                         filter(ori_type == "nonschool"), c("sexual_assault_per25"), explanatory_vars, ., "ori")
)


