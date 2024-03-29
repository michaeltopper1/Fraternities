## Purpose of script: Robustness check including 14 never-treated schools. 
##
## Author: Michael Topper
##
## Date Last Edited: 2021-12-13
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)
library(fwildclusterboot)



## for bootstrapping
# set seed via dqset.seed for boot_algo = "R" & Rademacher, Webb & Normal weights
# set 'familiar' seed for all other algorithms and weight types 
set.seed(23325, sample.kind = "Rejection")

daily_crime_allschools<- read_csv("created_data/xmaster_data/daily_panel_allschools.csv") %>% 
  filter((university %in% ifc::moratorium_schools()) | (university %in% ifc::never_treated_no_death()))

daily_crime_allschools_weekends <- daily_crime_allschools %>% 
  filter(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun")

daily_crime_allschools_weekdays <- daily_crime_allschools %>% 
  filter(!(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun"))

explanatory_vars <- c("treatment")
fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")


# fixed_effects_preferred_l <-  c("day_of_week", "university_by_academic_year_by_semester", "holiday")
data_subsets <- list(daily_crime_allschools, daily_crime_allschools_weekends, daily_crime_allschools_weekdays)

alc_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("alcohol_offense_per25"),explanatory_vars, fixed_effects_preferred, "university")
)


sex_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("sexual_assault_per25"),explanatory_vars, fixed_effects_preferred, "university")
)



# getting bootstrapped regressions ----------------------------------------
w_alc_1<- feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                data = daily_crime_allschools, cluster = "university")

wboot_alc_1 <- boottest(w_alc_1, clustid = "university", param = "treatment", B = 1000)

w_alc_2 <- feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                 data = daily_crime_allschools_weekends, cluster = "university")

wboot_alc_2 <- boottest(w_alc_2, clustid = "university", param = "treatment", B = 1000)

w_alc_3 <- feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                 data = daily_crime_allschools_weekdays, cluster = "university")

wboot_alc_3 <- boottest(w_alc_3, clustid = "university", param = "treatment", B = 1000)


# bootstrapped regressions for alcohol ------------------------------------
w_sex_1 <- feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                 data = daily_crime_allschools, cluster = "university")

wboot_sex_1 <- boottest(w_sex_1, clustid = "university", param = "treatment", B = 1000)

w_sex_2 <- feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                 data = daily_crime_allschools_weekends, cluster = "university")

wboot_sex_2 <- boottest(w_sex_2, clustid = "university", param = "treatment", B = 1000)

w_sex_3 <- feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                 data = daily_crime_allschools_weekdays, cluster = "university")

wboot_sex_3 <- boottest(w_sex_3, clustid = "university", param = "treatment", B = 1000)






weekend_table_allschools <- ifc::main_table(alc_weeksplit, last_panel = sex_weeksplit) %>% 
  slice(1:6) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime_allschools$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_allschools_weekends$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_allschools_weekdays$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Wild Bootstrap P-Value", 
          `Model 1` = sprintf("%.3f",wboot_alc_1$p_val),
          `Model 2` = sprintf("%.3f",wboot_alc_2$p_val),
          `Model 3` = sprintf("%.3f",wboot_alc_3$p_val),
          .before = 5) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime_allschools$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_allschools_weekends$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_allschools_weekdays$sexual_assault_per25, na.rm = T)),
          .before = 9) %>% 
  add_row(term = "Wild Bootstrap P-Value", 
          `Model 1` = sprintf("%.3f",wboot_sex_1$p_val),
          `Model 2` = sprintf("%.3f",wboot_sex_2$p_val),
          `Model 3` = sprintf("%.3f",wboot_sex_3$p_val),
          .before = 10) %>% 
  kbl(booktabs = T, col.names = c(" ", "All Days", "Weekends", "Weekdays"),
      caption = "\\label{weekend_table_allschools}Effect of Moratoriums on Alcohol Offenses and Sexual Assault by Weekend/Weekdays. Never-treated schools included. (OLS)") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 5, bold = F, italic = T) %>% 
  pack_rows("Panel B: Sexual Assaults", 6, 10, bold = F, italic = T,latex_gap_space = "0.5cm") %>% 
  add_header_above(c(" " = 1, "Days of the Week" = 3)) %>% 
  column_spec(1, width = "8cm") %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as per-25000 enrolled students. 14 never-treated schools are included in the sample for additional power. A never-treated schools is defined as a university that does not experience a moratorium in the time period of 2014-2019 and was included on the Top 50 fraternity schools on niche.com. See link here: https://www.niche.com/colleges/search/best-greek-life-colleges/. Weekends consist of Fridays, Saturdays, and Sundays. Weekdays consist of Monday through Thursday. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since not in panel. A moratorium is a temporary halt on fraternity-related activities with alcohol. ",
                "* p < 0.1, ** p < 0.05, *** p < 0.01"), threeparttable = T) %>% 
  kable_styling(latex_options = "HOLD_position")

