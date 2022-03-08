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



nibrs_spillovers <- map(nibrs_list, ~ifc::reghdfe(., c("alcohol_arrest_total_per25"), "treatment", fixed_effects_preferred_nibrs, "university"))



nibrs_half <- ifc::main_table(last_panel = nibrs_spillovers) %>% 
  add_row(term = "FE: University by Academic Year", `Model 1` = " ", `Model 2` = " ", `Model 3` = " ")
dcl_half <- ifc::main_table(last_panel = dcl_spillovers) %>% 
  add_row(term = "FE: Agency by Academic Year", `Model 1` = " ", `Model 2` = " ", `Model 3` = " ", .before = 8) %>% 
  rename("model_1" = `Model 1`, "model_2" = `Model 2`, "model_3" = `Model 3` ) %>% 
  select(-term)

spillover_table <- nibrs_half %>% 
  bind_cols(dcl_half) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(nibrs_treated_nonschool$alcohol_arrest_total_per25, na.rm = T)), 
          `Model 2` = sprintf("%.3f",mean(nibrs_treated_nonschool_weekends$alcohol_arrest_total_per25, na.rm = T)), 
          `Model 3` = sprintf("%.3f",mean(nibrs_treated_nonschool_weekdays$alcohol_arrest_total_per25, na.rm = T)), 
          model_1 = sprintf("%.3f",mean(daily_crime_spillover_schools$alcohol_offense_per25, na.rm = T)),
          model_2 = sprintf("%.3f",mean(daily_crime_spillover_schools_weekends$alcohol_offense_per25, na.rm = T)),
          model_3 = sprintf("%.3f",mean(daily_crime_spillover_schools_weekdays$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  kbl(booktabs = T, col.names = c(" ", "All Days", "Weekends", "Weekdays", "All Days", "Weekends", "Weekdays"),
      caption = "\\label{spillover_table} Effect of Moratoriums in Local Police Departments Compared to University Police Departments (OLS)") %>% 
  kable_styling() %>% 
  add_header_above(c(" " = 1, "Local Police Departments" = 3, "University Police Departments" = 3)) %>% 
  footnote(list("Offenses are per-25000 enrolled students. The local police departments were matched using the files from Lindo et. al 2018 paper. These police departments represent police departments that are nearby the universities where students/police may report crimes. Only 9 local police departments consistently reported data to the NIBRS, hence only 9 are included here. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since no university's academic calendar contains them. A moratorium is a temporary halt on fraternity-related activities with alcohol.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"),
           threeparttable = T)
