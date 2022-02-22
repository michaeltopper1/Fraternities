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



## using all oris to replicate lindo.
lindo_fixed_effects_1 <- c("day_of_week", "year", "ori", "holiday")
lindo_fixed_effects_2 <- c("day_of_week", "year", "ori_by_month", "holiday")
lindo_fixed_effects_3 <- c("day_of_week", "year", "ori_by_week", "holiday")
lindo_fixed_effects_4 <- c("day_of_week", "ori_by_month_by_week", "holiday")

lindo_fixed_effects_ori <- list(lindo_fixed_effects_1, lindo_fixed_effects_2, lindo_fixed_effects_3, lindo_fixed_effects_4)

explanatory_var <- "game_occurred"


alc_arrest <- map(lindo_fixed_effects_ori, ~ifc::reghdfe_pois(nibrs_treated, "alcohol_arrest_total", explanatory_var, ., "ori")) 

alc_arrest_college <- map(lindo_fixed_effects_ori, ~ifc::reghdfe_pois(nibrs_treated, "alcohol_arrest_college_aged", explanatory_var, ., "ori")) 

sex <- map(lindo_fixed_effects_ori, ~ifc::reghdfe_pois(nibrs_treated, "sexual_assault", "game_occurred", ., "ori")) 

sex_college <- map(lindo_fixed_effects_ori, ~ifc::reghdfe_pois(nibrs_treated, "college_age_sexual_assault", "game_occurred", ., "ori")) 


lindo_rep_nibrs <- ifc::main_table(alc_arrest , last_panel = sex) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated$alcohol_arrest_total, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(nibrs_treated$alcohol_arrest_total, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(nibrs_treated$alcohol_arrest_total, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(nibrs_treated$alcohol_arrest_total, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated$sexual_assault, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(nibrs_treated$sexual_assault, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(nibrs_treated$sexual_assault, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(nibrs_treated$sexual_assault, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)","(4)"),
      digits = 3,
      caption = "\\label{lindo_rep_nibrs}Effect of Game Day on Alcohol Arrests and Sexual Assaults (Poisson).") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  pack_rows("Panel A: Alcohol Arrests", 1, 4, bold = T, italic = F) %>%
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B:",9, 15, bold = T, italic = F ) %>% 
  footnote(list("Standard errors clustered by agency. Alcohol arrests and sexual assaults are counts. Sexual assaults is the sum of rape, fondling, statutory rape, and sexual assault with an object.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"), threeparttable = T)

lindo_rep_nibrs_college_age <- ifc::main_table(alc_arrest_college , last_panel = sex_college) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated$alcohol_arrest_college_aged, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(nibrs_treated$alcohol_arrest_college_aged, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(nibrs_treated$alcohol_arrest_college_aged, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(nibrs_treated$alcohol_arrest_college_aged, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated$college_age_sexual_assault, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(nibrs_treated$college_age_sexual_assault, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(nibrs_treated$college_age_sexual_assault, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(nibrs_treated$college_age_sexual_assault, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)","(4)"),
      digits = 3,
      caption = "\\label{lindo_rep_nibrs_college_age}Effect of Game Day on Alcohol Arrests and Sexual Assaults College Aged Only (Poisson).") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  pack_rows("Panel A: Alcohol Arrests", 1, 4, bold = T, italic = F) %>%
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B:",9, 15, bold = T, italic = F ) %>% 
  footnote(list("Standard errors clustered by agency. Alcohol arrests and sexual assaults are counts. Sexual assaults is the sum of rape, fondling, statutory rape, and sexual assault with an object. College aged is victim of 17-22 age.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"), threeparttable = T)

