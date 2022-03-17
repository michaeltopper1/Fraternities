library(tidyverse)
library(fixest) 
library(modelsummary)
library(ifc)
library(lubridate)
library(kableExtra)

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

ifc_regs_alc <- map(datas, ~ifc::reghdfe(.,  "alcohol_offense_per25", "treatment:ifc_enacted", c("day_of_week", "university_by_academic_year", "holiday", "spring_semester","game_occurred"), "university"))

ifc_regs_sex <- map(datas, ~ifc::reghdfe(.,  "sexual_assault_per25", "treatment:ifc_enacted", c("day_of_week", "university_by_academic_year", "holiday", "spring_semester","game_occurred"), "university"))



# university enacted regressions ------------------------------------------

uni_regs_alc <- map(datas, ~ifc::reghdfe(.,  "alcohol_offense_per25", "treatment:university_enacted", c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred"), "university"))

uni_regs_sex <- map(datas, ~ifc::reghdfe(.,  "sexual_assault_per25", "treatment:university_enacted", c("day_of_week", "university_by_academic_year", "holiday", "spring_semester","game_occurred"), "university"))



ifc_uni_table <- ifc::main_table(uni_regs_alc, uni_regs_sex,
                ifc_regs_alc, last_panel =  ifc_regs_sex) %>% 
  slice(1:12) %>% 
  kbl(booktabs = T, col.names = c(" ", "All Days", "Weekends", "Weekdays"), caption = "\\label{ifc_uni_table}Effect of Moratoriums Imposed by the University vs. the IFC") %>% 
kable_paper() %>% 
  pack_rows("Panel A: University-Enacted Moratoriums", 1, 6, hline_after = T) %>% 
  pack_rows("Panel B: IFC-Enacted Moratoriums", 7, 12, hline_after = T) %>% 
  pack_rows("Alcohol Offense", 1, 3, italic = T, bold = F) %>% 
  pack_rows("Sexual Assault", 4,6, italic = T, bold = F) %>% 
  pack_rows("Alcohol Offense", 7, 9, italic = T, bold = F) %>% 
  pack_rows("Sexual Assault",10, 12, italic = T, bold = F ) %>% 
  add_header_above(c(" " = 1, "Days of the Week" = 3)) %>% 
  footnote(list("Standard errors clustered by university. Controls follow specification (2) in the main results table with day of week, holiday, semester, and universitiy by academic year fixed effects. Panel A shows the effects of a moratorium when a moratorium is imposed by the university. University-imposed moratoriums represent 28/45 (62%) of the moratoriums. Panel B shows the effects of a moratorium when the IFC council imposes the moratorium. This is a student-lead initiative. IFC-imposed moratoriums represent 17/45 (38%) of the moratoriums in the sample. Weekends represent Fridays through Sundays while Weekdays represent Mondays through Thursdays.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"),
           threeparttable = T)



