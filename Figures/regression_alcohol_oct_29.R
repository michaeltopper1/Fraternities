## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-25
##

library(tidyverse)
library(kableExtra)
library(lubridate)
library(modelsummary)
library(fixest)


if (!exists("daily_crime")) {
  daily_crime <- read_csv(here::here("Created Data/xMaster_data_2021/daily_panel.csv"))
    # filter(university != "University of Kansas") %>% 
    # filter(university != "University of New Mexico-Main Campus") 
}

if (!exists("daily_crime_weekdays")) {
  daily_crime_weekdays <- daily_crime %>% 
    filter(day_of_week == "Mon" | day_of_week == "Thu" | day_of_week == "Wed" | day_of_week == "Tue" )
}

if (!exists("daily_crime_weekends")) {
  daily_crime_weekends <- daily_crime %>%
    filter(day_of_week == 'Fri' | day_of_week == "Sat" | day_of_week == "Sun")
}
daily_crime <- daily_crime %>% 
  mutate(week_before_2 = lead(week_before, 7)) 

# regressions full sample-------------------------------------------------------------
alc_ols_1 <- daily_crime %>% 
  feols(alcohol_offense_per25 ~ week_before +  treatment + week_after | date + year + university,
        vcov = cluster ~ university, data = .)

alc_ols_2 <- daily_crime %>% 
  feols(alcohol_offense_per25 ~ week_before + treatment + week_after | day_of_week  + university + semester_number,
        cluster = ~university, data = .)

alc_ols_3 <- daily_crime %>% 
  feols(alcohol_offense_per25 ~ week_before + treatment + week_after | day_of_week + university_by_semester_number ,
        cluster = ~university, data = .)




alc_ols <- list("(1)" = alc_ols_1, "(2)" = alc_ols_2, "(3)" = alc_ols_3)


# regressions weekends ----------------------------------------------------


alc_weekend_1 <- daily_crime_weekends %>% 
  feols(alcohol_offense_per25 ~ week_before + treatment + week_after | date + university,
        cluster = ~university, data = .) 
alc_weekend_2 <- daily_crime_weekends %>% 
  feols(alcohol_offense_per25 ~ week_before + treatment + week_after | day_of_week + university + semester_number,
        cluster = ~university, data = .) 
alc_weekend_3 <- daily_crime_weekends %>% 
  feols(alcohol_offense_per25 ~week_before +  treatment + week_after | day_of_week + university_by_semester_number,
        cluster = ~university, data = .)

alc_ols_weekends <- list("(1)" = alc_ols_1, "(2)" = alc_ols_2, "(3)" = alc_ols_3)
# regressions weekdays ----------------------------------------------------


alc_weekdays_1 <- daily_crime_weekdays %>% 
  feols(alcohol_offense_per25 ~ week_before + treatment + week_after| date + university,
        cluster = ~university, data = .) 
alc_weekdays_2 <- daily_crime_weekdays %>% 
  feols(alcohol_offense_per25 ~ week_before + treatment + week_after |day_of_week + university + semester_number,
        cluster = ~university, data = .) 
alc_weekdays_3 <- daily_crime_weekdays %>% 
  feols(alcohol_offense_per25 ~ week_before + treatment + week_after | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
alc_weekends <- list("(1)" = alc_weekend_1,"(2)"= alc_weekend_2, "(3)" = alc_weekend_3)
alc_weekdays <- list("(1)" =  alc_weekdays_1, "(2)" = alc_weekdays_2,"(3)" = alc_weekdays_3)

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-Number", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: year", "FE: Year", ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
              "FE: date", "FE: Day-by-Month-by-Year", ~fmt)

alc_full <- list("(1)" = alc_ols_1, "(2)" = alc_ols_2, "(3)" = alc_ols_3,
                  "(1)" = alc_weekend_1,"(2)"= alc_weekend_2, "(3)" = alc_weekend_3,
                 "(1)" =  alc_weekdays_1, "(2)" = alc_weekdays_2,"(3)" = alc_weekdays_3)

find_mean <- function(data, column) {
  column_mean <- data %>% 
    summarize(mean({{column}}, na.rm = T)) %>% 
    pull()
  return(column_mean)
}

add_means <- tribble(~term, ~alc_full, ~alc_full, ~alc_full, ~alc_weekend, ~alc_weekend, ~alc_weekend, ~alc_weekday, ~alc_weekday,~alc_weekday,
                     "Mean of Dependent Variable", find_mean(daily_crime, alcohol_offense_per25), find_mean(daily_crime, alcohol_offense_per25), find_mean(daily_crime, alcohol_offense_per25),
                     find_mean(daily_crime_weekends, alcohol_offense_per25), find_mean(daily_crime_weekends, alcohol_offense_per25), find_mean(daily_crime_weekends, alcohol_offense_per25),
                     find_mean(daily_crime_weekdays, alcohol_offense_per25), find_mean(daily_crime_weekdays, alcohol_offense_per25), find_mean(daily_crime_weekdays, alcohol_offense_per25))

attr(add_means, 'position') <- c(8)   
alc_table <- modelsummary(alc_full, stars = T,
             gof_omit = 'DF|Deviance|AIC|BIC|Log|R2|St',
             coef_map = c("week_before" = "Week Before",
                          "treatment" = "Moratorium",
                          "week_after" = "Week After"),
             title = "\\label{alc_offense}Effect of Moratoriums on Alcohol Offenses.",
             notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                          "Standard errors are clustered by university.",
                          "Outcome of interest is alcohol offenses per 25 thousand enrolled students.",
                          "Coefficient estimates shown are for Moratorium.",
                          "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
             gof_map = gm,
             add_rows = add_means) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 3, "Weekends (Fri-Sat)" = 3, "Weekdays (Mon-Thurs)" = 3))
broom::glance(alc_ols_1)

