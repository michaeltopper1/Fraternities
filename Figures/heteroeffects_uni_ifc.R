library(tidyverse)
library(fixest) 
library(modelsummary)
library(ifc)
library(lubridate)
library(kableExtra)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv") %>% 
    filter(university %in% ifc::moratorium_schools())
}

daily_crime <- daily_crime %>% 
  mutate(ifc_enacted = ifelse(university_enacted == 0 & treatment == 1, 1, 0))

daily_crime_weekdays<- daily_crime %>% 
  filter(weekday != "Fri" & weekday != "Sat" & weekday != "Sun")

daily_crime_weekends <- daily_crime %>% 
  filter(weekday == "Fri" | weekday == "Sat" | weekday == "Sun")


uni_enacted_alc <- daily_crime %>% 
  feols(alcohol_offense_per25 ~treatment:university_enacted + treatment:ifc_enacted| uni_semester + weekday,
        cluster = ~university, data = .)

uni_enacted_alc_weekend <- daily_crime_weekends %>% 
  feols(alcohol_offense_per25~treatment:university_enacted+ treatment:ifc_enacted| uni_semester + weekday,
        cluster = ~university, data = .)

uni_enacted_alc_weekdays <- daily_crime_weekdays %>% 
  feols(alcohol_offense_per25 ~treatment:university_enacted + treatment:ifc_enacted | uni_semester + weekday,
        cluster = ~university, data = .)

uni_enacted_sex <- daily_crime %>% 
  feols(sexual_assault_per25 ~treatment:university_enacted + treatment:ifc_enacted| uni_semester + weekday,
        cluster = ~university, data = .)

uni_enacted_sex_weekend <- daily_crime_weekends %>% 
  feols(sexual_assault_per25~treatment:university_enacted + treatment:ifc_enacted| uni_semester + weekday,
        cluster = ~university, data = .)

uni_enacted_sex_weekdays <- daily_crime_weekdays %>% 
  feols(sexual_assault_per25 ~treatment:university_enacted + treatment:ifc_enacted| uni_semester + weekday,
        cluster = ~university, data = .)

uni_enacted <- list("Full Sample" = uni_enacted_alc,
                            "Weekends" = uni_enacted_alc_weekend,
                            "Weekdays" = uni_enacted_alc_weekdays,
                            "Full Sample" = uni_enacted_sex,
                            "Weekends" = uni_enacted_sex_weekend,
                            "Weekdays" = uni_enacted_sex_weekdays)

full_means <- daily_crime %>% 
  summarize(alcohol_mean = mean(alcohol_offense_per25, na.rm = T),
            sex_mean = mean(sexual_assault_per25, na.rm = T)) %>% 
  mutate(across(everything(), ~round(.,4)))
weekend_means <- daily_crime_weekends %>% 
  summarize(alcohol_mean = mean(alcohol_offense_per25, na.rm = T),
            sex_mean = mean(sexual_assault_per25, na.rm = T)) %>% 
  mutate(across(everything(), ~round(.,4)))
weekday_means <- daily_crime_weekdays %>% 
  summarize(alcohol_mean = mean(alcohol_offense_per25, na.rm = T),
            sex_mean = mean(sexual_assault_per25, na.rm = T)) %>% 
  mutate(across(everything(), ~round(.,4)))

row_means <- tribble(~term, ~alc, ~alc_weeknd, ~alc_weekday, ~sex, ~sex_weekend, ~sex_weekday,
                     'Mean of Outcome',full_means[[1]], weekend_means[[1]], weekday_means[[1]], full_means[[2]], weekend_means[[2]], weekday_means[[2]])
attr(row_means, 'position') <- c(6)


uni_enacted_alc <- uni_enacted %>% modelsummary(stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log|R2 Within|R2 Ps|R2|R2 Adj.',
                             coef_map = c("treatment:university_enacted" = "Moratorium x University Enacted",
                                          "treatment:ifc_enacted" = "Moratorium x IFC Enacted"),
                             title = "Heterogeneous Effects for University-enacted Moratoriums and IFC-enacted Moratoriums.",
                             add_rows = row_means,
                             notes = list("IFC-enacted moratorium is a student-enforced moratorium.",
                                          "University-enacted moratorium is a university-enforced moratorium.",
                                          "Alcohol offenses are per-25,000 enrolled students",
                                          "Sexual assaults are per-25,000 enrolled students",
                                          "Full Sample includes every day of the week (Monday-Sunday).",
                                          "Weekends include Friday, Saturday, and Sunday.",
                                          "Weekdays include Monday, Tuesday, Wendesday, Thursday.")) %>% 
  add_header_above(c(" " = 1, "(1)" = 1, "(2)" = 1, "(3)"= 1,
                     "(4)" =1, "(5)" = 1, "(6)" = 1)) %>% 
  add_header_above(c(" " = 1, "Alcohol Offenses" = 3, "Sexual Assault" = 3))



