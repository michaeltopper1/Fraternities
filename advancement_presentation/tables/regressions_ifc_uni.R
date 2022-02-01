library(tidyverse)
library(fixest) 
library(modelsummary)
library(ifc)
library(lubridate)
library(gt)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}

if(!exists("daily_crime_weekends")) {
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv") 
}

if(!exists("daily_crime_weekdays")) {
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv") 
}



offenses <- list("alcohol_offense_per25", "drug_offense_per25", "sexual_assault_per25")

explanatory_vars <- c(
  "treatment:ifc_enacted",
  "treatment:university_enacted")

datas <- list(daily_crime, daily_crime_weekends, daily_crime_weekdays)


# ifc enacted regressions -------------------------------------------------

ifc_regs_alc <- map(datas, ~ifc::reghdfe(.,  "alcohol_offense_per25", "treatment:ifc_enacted", c("day_of_week", "university_by_academic_year", "holiday", "spring_semester"), "university"))

ifc_regs_sex <- map(datas, ~ifc::reghdfe(.,  "sexual_assault_per25", "treatment:ifc_enacted", c("day_of_week", "university_by_academic_year", "holiday", "spring_semester"), "university"))



# university enacted regressions ------------------------------------------

uni_regs_alc <- map(datas, ~ifc::reghdfe(.,  "alcohol_offense_per25", "treatment:university_enacted", c("day_of_week", "university_by_academic_year", "holiday", "spring_semester"), "university"))

uni_regs_sex <- map(datas, ~ifc::reghdfe(.,  "sexual_assault_per25", "treatment:university_enacted", c("day_of_week", "university_by_academic_year", "holiday", "spring_semester"), "university"))


ifc_uni_alc <- ifc::main_table(uni_regs_alc, uni_regs_sex,
                ifc_regs_alc, last_panel =  ifc_regs_sex) %>% 
  slice(1:3,7:9 ) %>% 
  gt() %>% 
  tab_header(title = "Effect of Moratoriums Imposed by the University vs. the IFC",
             subtitle = "Alcohol Offenses") %>% 
  cols_label(term = " ", `Model 1` = "All Days", `Model 2` = "Weekends", `Model 3` = "Weekdays") %>% 
  tab_spanner(label = 'Days of the Week', columns = c(2:4)) %>% 
  tab_row_group("IFC-Imposed Moratoriums", rows = c(4:6)) %>% 
  tab_row_group("University-Imposed Moratoriums", rows = c(1:3)) %>% 
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) %>% 
  tab_source_note(list("Standard errors clustered by university. University-imposed moratoriums represent 28/45 (62%) of the moratoriums. IFC-imposed moratoriums represent 17/45 (38%) of the moratoriums in the sample.",
                       "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

ifc_uni_sex <- ifc::main_table(uni_regs_alc, uni_regs_sex,
                ifc_regs_alc, last_panel =  ifc_regs_sex) %>% 
  slice(4:6,10:12) %>% 
  gt() %>% 
  tab_header(title = "Effect of Moratoriums Imposed by the University vs. the IFC",
             subtitle = "Sexual Assaults") %>% 
  cols_label(term = " ", `Model 1` = "All Days", `Model 2` = "Weekends", `Model 3` = "Weekdays") %>% 
  tab_spanner(label = 'Days of the Week', columns = c(2:4)) %>% 
  tab_row_group("IFC-Imposed Moratoriums", rows = c(4:6)) %>% 
  tab_row_group("University-Imposed Moratoriums", rows = c(1:3)) %>% 
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) %>% 
  tab_source_note(list("Standard errors clustered by university. University-imposed moratoriums represent 28/45 (62%) of the moratoriums. IFC-imposed moratoriums represent 17/45 (38%) of the moratoriums in the sample.",
                       "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))
