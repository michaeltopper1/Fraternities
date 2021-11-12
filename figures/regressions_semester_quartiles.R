## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-11-04
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)


if (!exists("semester_level")) {
  semester_level <- read_csv("created_data/xmaster_data/semester_level.csv")
}

if (!exists("semester_level_weekdays")){
  semester_level_weekdays <- read_csv("created_data/xmaster_data/semester_level_weekdays.csv")
}

if (!exists("semester_level_weekends")) {
  semester_level_weekends <- read_csv("created_data/xmaster_data/semester_level_weekends.csv")
  
}




quartile_semester_weekend <- semester_level_weekends %>% 
  feols(c(alcohol_offense_per25, drug_offense_per25, sexual_assault_per25) ~lead_2 + lead_1 + treatment :quartile_1 + treatment:quartile_2 +
          treatment:quartile_3 + treatment:quartile_4 +lag_1 + lag_2| university_by_semester_number + semester_number,
        cluster = ~university, data = .)

quartile_semester <- semester_level %>%
  feols(c(alcohol_offense_per25, drug_offense_per25, sexual_assault_per25) ~lead_2 + lead_1 + treatment :quartile_1 + treatment:quartile_2 +
          treatment:quartile_3 + treatment:quartile_4 +lag_1 + lag_2| university_by_semester_number + semester_number,
        cluster = ~university, data = .)

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs.", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-by-Year", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: year", "FE: Year", ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
              "FE: date", "FE: Day-by-Month-by-Year", ~fmt,
              "FE: university_by_year_by_semester_number", "FE: University-by-Year-by-Semester-Number", ~fmt)

names(quartile_semester) <- c("Alcohol", "Drug", "Sexual Assault")
names(quartile_semester_weekend) <- c("Alcohol", "Drug", "Sexual Assault")

quartile_semester_table_weekends <- modelsummary(quartile_semester_weekend, stars = T,gof_omit = 'DF|Deviance|AIC|BIC|Log|R2 Within|R2 Ps|R2|R2 Adj.',
             coef_rename = c("treatment:university_enacted" = "Moratorium x University Enacted",
                          "treatment:ifc_enacted" = "Moratorium x IFC Enacted",
                          "lead_2" = "2 Semesters Before",
                          "lead_1" = "1 Semester Before",
                          "lag_1" = "1 Semester After",
                          "lag_2" = "2 Semesters After"),
             gof_map = gm,
             title = "\\label{quartile_semester_weekend}Effects of moratorium by fraction of days in a semester treated. Split by quartiles. (Weekends)",
             notes = list("Quartile 1 = 0-25th percentile of all fraction of treated semester days.",
                          "Quartile 2 = 25th - 50th.",
                          "Quartile 3 = 50th - 75th",
                          "Quartile 4 = 75th - 100th"))

quartile_semester_table <- modelsummary(quartile_semester_weekend, stars = T,gof_omit = 'DF|Deviance|AIC|BIC|Log|R2 Within|R2 Ps|R2|R2 Adj.',
             coef_rename = c("treatment:university_enacted" = "Moratorium x University Enacted",
                             "treatment:ifc_enacted" = "Moratorium x IFC Enacted",
                             "lead_2" = "2 Semesters Before",
                             "lead_1" = "1 Semester Before",
                             "lag_1" = "1 Semester After",
                             "lag_2" = "2 Semesters After"),
             gof_map = gm,
             title = "\\label{quartile_semester}Effects of moratorium by fraction of days in a semester treated. Split by quartiles. (Full Sample)",
             notes = list("Quartile 1 = 0-25th percentile of all fraction of treated semester days.",
                          "Quartile 2 = 25th - 50th.",
                          "Quartile 3 = 50th - 75th",
                          "Quartile 4 = 75th - 100th"))

