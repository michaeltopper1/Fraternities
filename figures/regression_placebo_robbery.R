## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-07-19
##

library(tidyverse)
library(lubridate)
library(fixest)
library(kableExtra)
library(modelsummary)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv") %>% 
    filter(university %in% ifc::moratorium_schools())
}

if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv")
}

explanatory_vars <- c("treatment")
fe <- c("university", "date")

data <- list(daily_crime, daily_crime_weekends, daily_crime_weekends)

robbery <- map(data, ~ifc::reghdfe(., "robbery_burglary_per25", explanatory_vars, fe, "university")) 

names(robbery) <- c("Full Sample", "Weekends", "Weekdays")



robbery_means <- map_df(data, ~.x %>% summarize(avg_robbery = mean(robbery_burglary_per25, na.rm = T)))



row_means <- tribble(~term, ~rob, ~rob_weekend, ~rob_weekdays,
                     'Mean of Outcome',robbery_means[[1,1]], robbery_means[[2,1]], robbery_means[[3,1]])
attr(row_means, 'position') <- c(4)

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs.", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-by-Year", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: year", "FE: Year", ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
              "FE: date", "FE: Day-by-Month-by-Year", ~fmt,
              "FE: university_by_year_by_semester_number", "FE: University-by-Year-by-Semester-Number", ~fmt)

robbery_table <- modelsummary(robbery, stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
             coef_map = c("treatment" = "Moratorium"),
             title = "\\label{robbery_table}Effect of Fraternity Moratoriums on Robbery/Burglary",
             notes = list("Reports of robbery for ols regressions are per-25,000 students enrolled.",
             "Estimates include all days of the week (Mon-Sun).",
             "Standard errors clustered by university"),
             add_rows = row_means,
             gof_map = gm) 





