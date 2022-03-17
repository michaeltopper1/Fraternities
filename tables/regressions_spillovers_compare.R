## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-03-01
##

library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv") 
}

if (!exists("nibrs_treated_nonschool")) {
  nibrs_treated_nonschool <- read_csv("created_data/xmaster_data/nibrs_final.csv") %>% 
    filter(university %in% ifc::moratorium_schools()) %>% 
    filter(ori_type == "nonschool")
}

spillover_schools <- nibrs_treated_nonschool %>% 
  distinct(university) %>% pull()

daily_crime <- daily_crime %>% 
  mutate(home_game = ifelse(is.na(home_game), 0, home_game)) 

# creating weekend/weekday subsets for mapping ----------------------------

daily_crime_spillover_schools <- daily_crime %>% 
  filter(university %in% spillover_schools)

daily_crime_spillover_schools_weekends <- daily_crime_spillover_schools %>% 
  filter(day_of_week %in% c("Sat", "Fri", "Sun"))

daily_crime_spillover_schools_weekdays <- daily_crime_spillover_schools %>% 
  filter(!day_of_week %in% c("Sat", "Fri", "Sun"))


# creating weekend/weekday subsets for mapping for NIBRS ------------------

nibrs_treated_nonschool_weekends <- nibrs_treated_nonschool %>% 
  filter(day_of_week %in% c("Sat", "Fri", "Sun"))

nibrs_treated_nonschool_weekdays <- nibrs_treated_nonschool %>% 
  filter(!day_of_week %in% c("Sat", "Fri", "Sun"))


# creating lists to map through with the data frames ----------------------

nibrs_list <- list(nibrs_treated_nonschool, nibrs_treated_nonschool_weekends, nibrs_treated_nonschool_weekdays)

daily_crime_list <- list(daily_crime_spillover_schools, daily_crime_spillover_schools_weekends, daily_crime_spillover_schools_weekdays)



# creating fixed effects --------------------------------------------------

fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")
fixed_effects_preferred_nibrs <- c("day_of_week", "ori_by_academic_year", "holiday", "spring_semester", "game_occurred")

explanatory_vars <- c("treatment")


dcl_spillovers <- map(daily_crime_list, ~ifc::reghdfe(., c("alcohol_offense_per25"), "treatment", fixed_effects_preferred, "university"))



nibrs_spillovers <- map(nibrs_list, ~ifc::reghdfe(., c("alcohol_arrest_college_aged_per25"), "treatment", fixed_effects_preferred_nibrs, "university"))

dcl_spillovers_sex <- map(daily_crime_list, ~ifc::reghdfe(., c("sexual_assault_per25"), "treatment", fixed_effects_preferred, "university"))

nibrs_spillovers_sex <- map(nibrs_list, ~ifc::reghdfe(., c("college_age_sexual_assault_per25"), "treatment", fixed_effects_preferred_nibrs, "university"))


# combining nibrs to table ------------------------------------------------

nibrs_half_alc <- ifc::main_table(last_panel = nibrs_spillovers) %>% 
  add_row(term = "FE: University by Academic Year", `Model 1` = " ", `Model 2` = " ", `Model 3` = " ") %>% 
  slice(1:3)

nibrs_half_sex <- ifc::main_table(last_panel = nibrs_spillovers_sex) %>% 
  add_row(term = "FE: University by Academic Year", `Model 1` = " ", `Model 2` = " ", `Model 3` = " ")

nibrs_half_both <- bind_rows(nibrs_half_alc, nibrs_half_sex)

# combining dcl to table --------------------------------------------------

dcl_half_alc <- ifc::main_table(last_panel = dcl_spillovers) %>% 
  add_row(term = "FE: Agency by Academic Year", `Model 1` = " ", `Model 2` = " ", `Model 3` = " ", .before = 8) %>% 
  rename("model_1" = `Model 1`, "model_2" = `Model 2`, "model_3" = `Model 3` ) %>% 
  select(-term) %>% 
  slice(1:3)

dcl_half_sex <- ifc::main_table(last_panel = dcl_spillovers_sex) %>% 
  add_row(term = "FE: Agency by Academic Year", `Model 1` = " ", `Model 2` = " ", `Model 3` = " ", .before = 8) %>% 
  rename("model_1" = `Model 1`, "model_2" = `Model 2`, "model_3" = `Model 3` ) %>% 
  select(-term)

dcl_half_both <- bind_rows(dcl_half_alc, dcl_half_sex)

spillover_table <- nibrs_half_both %>% 
  bind_cols(dcl_half_both) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_total_per25, na.rm = T)), 
          `Model 2` = sprintf("%.3f",mean(nibrs_treated_nonschool_weekends$alcohol_arrest_total_per25, na.rm = T)), 
          `Model 3` = sprintf("%.3f",mean(nibrs_treated_nonschool_weekdays$alcohol_arrest_total_per25, na.rm = T)), 
          model_1 = sprintf("%.3f",mean(daily_crime_spillover_schools$alcohol_offense_per25, na.rm = T)),
          model_2 = sprintf("%.3f",mean(daily_crime_spillover_schools_weekends$alcohol_offense_per25, na.rm = T)),
          model_3 = sprintf("%.3f",mean(daily_crime_spillover_schools_weekdays$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated_nonschool$sexual_assault_per25, na.rm = T)), 
          `Model 2` = sprintf("%.3f",mean(nibrs_treated_nonschool_weekends$sexual_assault_per25, na.rm = T)), 
          `Model 3` = sprintf("%.3f",mean(nibrs_treated_nonschool_weekdays$sexual_assault_per25, na.rm = T)), 
          model_1 = sprintf("%.3f",mean(daily_crime_spillover_schools$sexual_assault_per25, na.rm = T)),
          model_2 = sprintf("%.3f",mean(daily_crime_spillover_schools_weekends$sexual_assault_per25, na.rm = T)),
          model_3 = sprintf("%.3f",mean(daily_crime_spillover_schools_weekdays$sexual_assault_per25, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, col.names = c(" ", "All Days", "Weekends", "Weekdays", "All Days", "Weekends", "Weekdays"),
      caption = "\\label{spillover_table} Effect of Moratoriums in Local Police Departments Compared to University Police Departments (OLS)") %>% 
  kable_styling() %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = T, italic = F) %>% 
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B:", 9, 14) %>% 
  add_header_above(c(" " = 1, "Nearby Police Departments" = 3, "University Police Departments" = 3)) %>% 
  footnote(list("Nearby Police Departments uses the NIBRS data which pertains to police departments that are closest to the university. University Police Departments uses the Daily Crime Log data set in which contains only university-specific police departments. Only 9 local police departments in the NIBRS data consistently report in the sample period. This table represents the comparison of alcohol offenses and sexual assaults per-25000 enrolled students at the nine local police departments and the corresponding nine universities. Standard errors are clustered by agency for NIBRS data and by university for Daily Crime Log data.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"),
           threeparttable = T)
