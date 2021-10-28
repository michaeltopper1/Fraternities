library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)
library(fwildclusterboot)
library(kableExtra)


# loading in data ---------------------------------------------------------

if (!exists("daily_crime")) {
  daily_crime <- read_csv(here::here("Created Data/xMaster_data_2021/daily_panel.csv")) 
}



if (!exists("daily_crime_weekdays")) {
  daily_crime_weekdays <- daily_crime %>% 
    filter(day_of_week == "Mon" | day_of_week == "Thu" | day_of_week == "Wed" | day_of_week == "Tue" )
}

if (!exists("daily_crime_weekends")) {
  daily_crime_weekends <- daily_crime %>%
    filter(day_of_week == 'Fri' | day_of_week == "Sat" | day_of_week == "Sun")
}


# regressions full sample-------------------------------------------------------------
drug_ols_1 <- daily_crime %>% 
  feols(drug_offense_per25 ~week_before +  treatment + week_after | date  + university,
        vcov = cluster ~ university, data = .)

drug_ols_2 <- daily_crime %>% 
  feols(drug_offense_per25 ~ week_before + treatment+ week_after | day_of_week + university + semester_number,
        cluster = ~university, data = .)

drug_ols_3 <- daily_crime %>% 
  feols(drug_offense_per25 ~ week_before + treatment + week_after| day_of_week + university_by_semester_number  ,
        cluster = ~university, data = .)




# regressions weekends ----------------------------------------------------


drug_weekend_1 <- daily_crime_weekends %>% 
  feols(drug_offense_per25 ~ week_before + treatment + week_after | date + university,
        cluster = ~university, data = .) 
drug_weekend_2 <- daily_crime_weekends %>% 
  feols(drug_offense_per25 ~ week_before + treatment + week_after | day_of_week + university + semester_number,
        cluster = ~university, data = .) 
drug_weekend_3 <- daily_crime_weekends %>% 
  feols(drug_offense_per25 ~week_before +  treatment  + week_after| day_of_week + university_by_semester_number ,
        cluster = ~university, data = .)


# regressions weekdays ----------------------------------------------------


drug_weekdays_1 <- daily_crime_weekdays %>% 
  feols(drug_offense_per25 ~week_before +  treatment + week_after | date + university,
        cluster = ~university, data = .) 
drug_weekdays_2 <- daily_crime_weekdays %>% 
  feols(drug_offense_per25 ~week_before + treatment + week_after| day_of_week + university + semester_number,
        cluster = ~university, data = .) 
drug_weekdays_3 <- daily_crime_weekdays %>% 
  feols(drug_offense_per25 ~ week_before + treatment + week_after| day_of_week + university_by_semester_number ,
        cluster = ~university, data = .)


drug_ols <- list("(1)" = drug_ols_1, "(2)" = drug_ols_2, "(3)" = drug_ols_3,
                "(1)" = drug_weekend_1, "(2)" = drug_weekend_2, "(3)" = drug_weekend_3, 
                "(1)" = drug_weekdays_1, "(2)" = drug_weekdays_2, "(3)" = drug_weekdays_3)

gm <- tribble(~raw, ~clean, ~fmt,
              "nobs", "Num.Obs", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-Number", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: year", "FE: Year", ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester-Number", ~fmt,
              "FE: date", "FE: Day-by-Month-by-Year", ~fmt)

drug_table <- modelsummary(drug_ols, stars = T,
             gof_omit = 'DF|Deviance|AIC|BIC|Log|R2|St',
             coef_map = c("week_before" = "Week Before",
                          "treatment" = "Moratorium",
                          "week_after" = "Week After"),
             title = "\\label{drug_offense}Effect of Moratoriums on Drug Offenses",
             notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                          "Standard errors are clustered by university.",
                          "Outcome of interest is drug offenses per 25 thousand enrolled students.",
                          "Coefficient estimates shown are for Moratorium.",
                          "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
             gof_map = gm) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 3, "Weekends (Fri-Sat)" = 3, "Weekdays (Mon-Thurs)" = 3))
