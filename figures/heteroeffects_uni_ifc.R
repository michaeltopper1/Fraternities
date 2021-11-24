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

ifc_uni_regs <- map(offenses, ~ifc::reghdfe(daily_crime,  ., explanatory_vars, c("university", "date"), "university"))

ifc_uni_regs_weekends <- map(offenses, ~ifc::reghdfe(daily_crime_weekends,  ., explanatory_vars, c("university", "date"), "university"))

ifc_regs <- c(ifc_uni_regs, ifc_uni_regs_weekends)


names(ifc_regs) <- c("Alcohol", "Drug", "Sexual Assault", "Alcohol", "Drug", "Sexual Assault")

full_means <- daily_crime %>% 
  summarize(alcohol_mean = mean(alcohol_offense_per25, na.rm = T),
            drug_mean = mean(drug_offense_per25, na.rm = T),
            sex_mean = mean(sexual_assault_per25, na.rm = T)) %>% 
  mutate(across(everything(), ~round(.,4)))
weekend_means <- daily_crime_weekends %>% 
  summarize(alcohol_mean = mean(alcohol_offense_per25, na.rm = T),
            drug_mean = mean(drug_offense_per25, na.rm = T),
            sex_mean = mean(sexual_assault_per25, na.rm = T)) %>% 
  mutate(across(everything(), ~round(.,4)))

row_means <- tribble(~term, ~alc, ~drug, ~sex, ~alc_w, ~drug_w, ~sex_w,
                     "Mean of Dependent Variable", full_means[[1,1]], full_means[[1,2]], full_means[[1,3]],
                     weekend_means[[1,1]], weekend_means[[1,2]], weekend_means[[1,3]])
attr(row_means, 'position') <- c(6)

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs.", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-by-Year", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: year", "FE: Year", ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
              "FE: date", "FE: Day-by-Month-by-Year", ~fmt,
              "FE: university_by_year_by_semester_number", "FE: University-by-Year-by-Semester-Number", ~fmt)


ifc_uni_table <- modelsummary(ifc_regs,
             stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log|R2 Within|R2 Ps|R2|R2 Adj.',
             coef_map = c("treatment:university_enacted" = "Moratorium x University Enacted",
                          "treatment:ifc_enacted" = "Moratorium x IFC Enacted"),
             title = "\\label{ifc_hetero}Heterogeneous Effects for University-enacted Moratoriums and IFC-enacted Moratoriums.",
             gof_map = gm,
             add_rows = row_means,
             notes = list("Standard errors clustered by university",
                          "Universitiy enacted is a moratorium enacted by university officials.",
                          "IFC enacted is a moratorium enacted by the student-IFC representatives.")) %>% 
  add_header_above(c(" "= 1, "Full Sample" = 3, "Weekends" = 3)) %>% 
  add_header_above(c(" " =1, "Dependent Variable" = 6))



