## Purpose of script: Robustness check including 14 never-treated schools. 
##
## Author: Michael Topper
##
## Date Last Edited: 2021-12-13
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

daily_crime_allschools<- read_csv("created_data/xmaster_data/daily_panel_allschools.csv") %>% 
  filter((university %in% ifc::moratorium_schools()) | (university %in% ifc::never_treated_no_death()))

daily_crime_allschools_weekends <- daily_crime_allschools %>% 
  filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun")

daily_crime_allschools_weekdays <- daily_crime_allschools %>% 
  filter(!(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun"))

explanatory_vars <- c("treatment")
fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester")


# fixed_effects_preferred_l <-  c("day_of_week", "university_by_academic_year_by_semester", "holiday")
data_subsets <- list(daily_crime_allschools, daily_crime_allschools_weekends, daily_crime_allschools_weekdays)

alc_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("alcohol_offense_per25"),explanatory_vars, fixed_effects_preferred, "university")
)

drug_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("drug_offense_per25"),explanatory_vars, fixed_effects_preferred, "university")
)

sex_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("sexual_assault_per25"),explanatory_vars, fixed_effects_preferred, "university")
)

weekend_table_allschools <- ifc::main_table(alc_weeksplit, drug_weeksplit, last_panel = sex_weeksplit) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime_allschools$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_allschools_weekends$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_allschools_weekdays$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime_allschools$drug_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_allschools_weekends$drug_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_allschools_weekdays$drug_offense_per25, na.rm = T)),
          .before = 8) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime_allschools$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_allschools_weekends$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_allschools_weekdays$sexual_assault_per25, na.rm = T)),
          .before = 12) %>% 
  kbl(booktabs = T, col.names = c(" ", "All Days", "Weekends", "Weekdays"),
      caption = "\\label{weekend_table_allschools}Effect of Moratoriums on Alcohol Offenses, Drug Offenses, and Sexual Assault by Weekend/Weekdays. Never-treated schools included.") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = T, italic = F) %>% 
  pack_rows("Panel B: Drug Offenses", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Panel C: Sexual Assaults", 9, 12, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-C:", 13, 16, bold = T, italic = F) %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as per-25000 enrolled students. 14 never-treated schools are included in the sample for additional power. A never-treated schools is defined as a university that does not experience a moratorium in the time period of 2014-2019 and was included on the Top 40 fraternity schools on niche.com. See link here: https://www.niche.com/colleges/search/best-greek-life-colleges/. Weekends consist of Fridays, Saturdays, and Sundays. Weekdays consist of Monday through Thursday. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since not in panel. A moratorium is a temporary halt on fraternity-related activities with alcohol. ",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"), threeparttable = T) 

