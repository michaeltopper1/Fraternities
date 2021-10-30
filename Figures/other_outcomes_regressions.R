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

daily_crime_weekdays<- daily_crime %>% 
  filter(day_of_week != "Fri" & day_of_week != "Sat" & day_of_week != "Sun")

daily_crime_weekends <- daily_crime %>% 
  filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun")


robbery <- daily_crime %>% 
  feols(robbery_burglary_per25 ~ treatment |
          university_by_semester_number + day_of_week, cluster = ~university, data = .)
drug <- daily_crime %>% 
  feols(drug_offense_per25 ~ treatment |
          university_by_semester_number + day_of_week, cluster = ~university, data = .)
theft <- daily_crime %>% 
  feols(theft_per25 ~ treatment |
          university_by_semester_number + day_of_week, cluster = ~university, data = .)

robbery_weekend <- daily_crime_weekends %>% 
  feols(robbery_burglary_per25 ~ treatment |
          university_by_semester_number + day_of_week, cluster = ~university, data = .)
drug_weekend <- daily_crime_weekends %>% 
  feols(drug_offense_per25 ~ treatment |
          university_by_semester_number + day_of_week, cluster = ~university, data = .)
theft_weekend <- daily_crime_weekends %>% 
  feols(theft_per25 ~ treatment |
          university_by_semester_number + day_of_week, cluster = ~university, data = .)


robbery_weekday <- daily_crime_weekdays %>% 
  feols(robbery_burglary_per25 ~ treatment |
          university_by_semester_number + day_of_week, cluster = ~university, data = .)
drug_weekday <- daily_crime_weekdays %>% 
  feols(drug_offense_per25 ~ treatment |
          university_by_semester_number + day_of_week, cluster = ~university, data = .)
theft_weekday <- daily_crime_weekdays %>% 
  feols(theft_per25 ~ treatment |
          university_by_semester_number + day_of_week, cluster = ~university, data = .)


other_models <- list("Robbery/Burglary" = robbery, "Drug Offense" = drug, "Theft" = theft,
                     "Robbery/Burglary" = robbery_weekend, "Drug Offense" = drug_weekend, "Theft" = theft_weekend,
                     "Robbery/Burglary" = robbery_weekday, "Drug Offense" = drug_weekday, "Theft" = theft_weekday)


get_means <- function(df, column) {
  column_mean <- df %>% 
    summarize(mean({{column}}, na.rm = T)) %>% 
    pull()
  return(column_mean)
}

row_means <- tribble(~term, ~rob, ~drug, ~theft, ~rob_weekend, ~drug_weekend, ~theft_weekend, ~rob_wday, ~drug_wday, ~theft_wday,
                     'Mean of Outcome Variable', get_means(daily_crime, robbery_burglary_per25),get_means(daily_crime, drug_offense_per25), get_means(daily_crime, theft_per25),
                     get_means(daily_crime_weekends, robbery_burglary_per25),get_means(daily_crime_weekends, drug_offense_per25),get_means(daily_crime_weekends, theft_per25),
                     get_means(daily_crime_weekdays, robbery_burglary_per25),get_means(daily_crime_weekdays, drug_offense_per25),get_means(daily_crime_weekdays, theft_per25))
attr(row_means, 'position') <- c(4)
gm <- tribble(~raw, ~clean, ~fmt,
              "FE: university_by_semester_number", "FE: University-by-Semester", ~fmt,
              "FE: weekday", "FE: Day-of-Week", ~fmt,
              "Std. Errors", "Std.Errors", ~fmt)
other_outcomes_table <- modelsummary(other_models, stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
                              coef_map = c("treatment" = "Moratorium",
                                           "ftime_total_undergrad" = "Fraction Full-time Undergrad",
                                           "frac_undergrad_black" = "Fraction Undergrad Black",
                                           "frac_undergrad_asian" = "Fraction Undergrad Asian",
                                           "frac_undergrad_hispanic_latino" = "Fraction Undergrad Hispanic",
                                           "graduation_rate_total_cohort" = "Graduation Rate",
                                           "university_by_semester_number_number" = "University by Semester"),
                              title = "\\label{otheroutcomes}Effect of Fraternity Moratoriums on other outcomes",
                         notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times",
                          "Standard errors are clustered by university.",
                          "Outcome of interest is alcohol offenses per 25 thousand students.",
                          "Coefficient estimates shown are for Moratorium.",
                          "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
                              add_rows = row_means) %>% 
  add_header_above(c(" " = 1, "Full Sample" = 3, "Weekends (Fri-Sat)" = 3, "Weekdays (Mon - Thurs)" = 3)) %>% 
  landscape() %>%
  kable_styling(latex_options = "scale_down")





