library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)
library(fwildclusterboot)
library(kableExtra)


# loading in data ---------------------------------------------------------

if (!exists("daily_crime")) {
  daily_crime <- read_csv(here::here("created_data/xmaster_data/daily_panel.csv")) 
}

daily_crime <- daily_crime %>% 
  mutate(sexual_assault_school = ifelse(reason1 %in% c("sexual assault") | reason2 %in% c("sexual assault"), 1,0)) %>%
  filter(sexual_assault_school == 0)


if (!exists("daily_crime_weekdays")) {
  daily_crime_weekdays <- daily_crime %>% 
    filter(day_of_week == "Mon" | day_of_week == "Thu" | day_of_week == "Wed" | day_of_week == "Tue" )
}

if (!exists("daily_crime_weekends")) {
  daily_crime_weekends <- daily_crime %>%
    filter(day_of_week == 'Fri' | day_of_week == "Sat" | day_of_week == "Sun")
}


# regressions full sample-------------------------------------------------------------
sex_ols_1 <- daily_crime %>% 
  feols(sexual_assault_per25 ~week_before +  treatment + week_after | date + university,
        vcov = cluster ~ university, data = .)

sex_ols_2 <- daily_crime %>% 
  feols(sexual_assault_per25 ~ week_before + treatment+ week_after | day_of_week + university + semester_number,
        cluster = ~university, data = .)

sex_ols_3 <- daily_crime %>% 
  feols(sexual_assault_per25 ~ week_before + treatment + week_after| day_of_week + university_by_semester_number  ,
        cluster = ~university, data = .)




# regressions weekends ----------------------------------------------------


sex_weekend_1 <- daily_crime_weekends %>% 
  feols(sexual_assault_per25 ~ week_before + treatment + week_after | date + university,
        cluster = ~university, data = .) 
sex_weekend_2 <- daily_crime_weekends %>% 
  feols(sexual_assault_per25 ~ week_before + treatment + week_after | day_of_week + university + semester_number,
        cluster = ~university, data = .) 
sex_weekend_3 <- daily_crime_weekends %>% 
  feols(sexual_assault_per25 ~week_before +  treatment  + week_after| day_of_week + university_by_semester_number ,
        cluster = ~university, data = .)


# regressions weekdays ----------------------------------------------------


sex_weekdays_1 <- daily_crime_weekdays %>% 
  feols(sexual_assault_per25 ~week_before +  treatment + week_after | date + year + university,
        cluster = ~university, data = .) 
sex_weekdays_2 <- daily_crime_weekdays %>% 
  feols(sexual_assault_per25 ~week_before + treatment + week_after| day_of_week + university + semester_number,
        cluster = ~university, data = .) 
sex_weekdays_3 <- daily_crime_weekdays %>% 
  feols(sexual_assault_per25 ~ week_before + treatment + week_after| day_of_week + university_by_semester_number ,
        cluster = ~university, data = .)


sex_ols <- list("(1)" = sex_ols_1, "(2)" = sex_ols_2, "(3)" = sex_ols_3,
                "(1)" = sex_weekend_1, "(2)" = sex_weekend_2, "(3)" = sex_weekend_3, 
                "(1)" = sex_weekdays_1, "(2)" = sex_weekdays_2, "(3)" = sex_weekdays_3)

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-Number", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: year", "FE: Year", ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
              "FE: date", "FE: Year-by-Month-by-Day", ~fmt)

sex_omit_sa <- modelsummary(sex_ols, stars = T,
             gof_omit = 'DF|Deviance|AIC|BIC|Log|R2|St',
             coef_map = c("week_before" = "Week Before",
                          "treatment" = "Moratorium",
                          "week_after" = "Week After"),
             title = "\\label{sex_offense_sa}Effect of Moratoriums on Sexual Assaults (omitted sexual assault schools)",
             notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                          "Standard errors are clustered by university.",
                          "Outcome of interest is sexua assaults per 25 thousand enrolled students.",
                          "Coefficient estimates shown are for Moratorium.",
                          "Sexual Assault Moratoriums are omitted in this sample",
                          "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
             gof_map = gm) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 3, "Weekends (Fri-Sat)" = 3, "Weekdays (Mon-Thurs)" = 3))
