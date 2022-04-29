library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)
library(fwildclusterboot)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv") 
}



# alcohol offense regresions ----------------------------------------------


main_alc_1 <- feols(alcohol_offense_per25 ~ treatment | day_of_week + university + academic_year + game_occurred + holiday + spring_semester,
                    data = daily_crime, cluster = "university")


boot_alc_1 <- boottest(main_alc_1, clustid = "university", param = "treatment", B = 9999)


main_alc_2 <- feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                    data = daily_crime, cluster = "university")

boot_alc_2 <- boottest(main_alc_2, clustid = "university", param = "treatment", B = 9999)

main_alc_3 <- feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_academic_year_by_semester + game_occurred + holiday,
                    data = daily_crime, cluster = "university")

boot_alc_3 <- boottest(main_alc_3, clustid = "university", param = "treatment", B = 9999)

## use this for fixed effects
alc <- list(main_alc_1, main_alc_2, main_alc_3)

## use this for boottable
boot_alc <- list(boot_alc_1, boot_alc_2, boot_alc_3)



# sexual assault regressions ----------------------------------------------


main_sex_1 <- feols(sexual_assault_per25 ~ treatment | day_of_week + university + academic_year + game_occurred + holiday + spring_semester,
                    data = daily_crime, cluster = "university")
boot_sex_1 <- boottest(main_sex_1, clustid = "university", param = "treatment", B = 9999)

main_sex_2 <- feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                    data = daily_crime, cluster = "university")

boot_sex_2 <- boottest(main_sex_2, clustid = "university", param = "treatment", B = 9999)

main_sex_3 <- feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_academic_year_by_semester + game_occurred + holiday,
                    data = daily_crime, cluster = "university")

boot_sex_3 <- boottest(main_sex_3, clustid = "university", param = "treatment", B = 9999)

## use this for fixed effects append
sex <- list(main_sex_1, main_sex_2, main_sex_3)

## use this for boot table
boot_sex <- list(boot_sex_1, boot_sex_2, boot_sex_3)



# appending together the table --------------------------------------------

main_table_bootstrap <- ifc::main_table_bootstrap(boot_alc, last_panel = boot_sex, display_fe = T)

fixed_effect_table <- ifc::main_table(alc, last_panel = sex) %>% 
  slice(7:n())


main_table_bootstrap <- main_table_bootstrap %>% 
  bind_rows(fixed_effect_table) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)"),
      digits = 3,
      caption = "\\label{main_table_bootstrap}Effect of Moratoriums on Alcohol Offenses and Sexual Assaults (OLS).") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = F, italic = T) %>%
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = F, italic = T) %>% 
  pack_rows("Controls for Panels A-B:",9, 16, bold = F, italic = T ) %>% 
  footnote(list("95% onfidence intervals are obtained through a wild cluster bootstrap by university and each offense is defined as per-25000 enrolled students. Weekends consist of Fridays, Saturdays, and Sundays. Weekdays consist of Monday through Thursday. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since these holiday's are not on any university's academic calendar. A moratorium is a temporary halt on fraternity-related activities with alcohol. Specification (2) is the preferred specification due to the flexibility of the fixed effects and the conservativeness of the estimates.",
                "* p < 0.1, ** p < 0.05, *** p < 0.01"), threeparttable = T)



# alcohol offense regresions ----------------------------------------------


w_alc_1 <- feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                    data = daily_crime, cluster = "university")


wboot_alc_1 <- boottest(w_alc_1, clustid = "university", param = "treatment", B = 9999)


w_alc_2 <- feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                    data = daily_crime_weekends, cluster = "university")

wboot_alc_2 <- boottest(w_alc_2, clustid = "university", param = "treatment", B = 9999)

w_alc_3 <- feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                    data = daily_crime_weekdays, cluster = "university")

wboot_alc_3 <- boottest(w_alc_3, clustid = "university", param = "treatment", B = 9999)


## use this for boottable
wboot_alc <- list(wboot_alc_1, wboot_alc_2, wboot_alc_3)



# sexual assault regressions ----------------------------------------------


w_sex_1 <- feols(sexual_assault_per25 ~ treatment |day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                    data = daily_crime, cluster = "university")
wboot_sex_1 <- boottest(w_sex_1, clustid = "university", param = "treatment", B = 9999)

w_sex_2 <- feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                    data = daily_crime_weekends, cluster = "university")

wboot_sex_2 <- boottest(w_sex_2, clustid = "university", param = "treatment", B = 9999)

w_sex_3 <- feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                    data = daily_crime_weekdays, cluster = "university")

wboot_sex_3 <- boottest(w_sex_3, clustid = "university", param = "treatment", B = 9999)


## use this for boot table
wboot_sex <- list(wboot_sex_1, wboot_sex_2, wboot_sex_3)

weekend_table_bootstrap <- ifc::main_table_bootstrap(wboot_alc, last_panel = wboot_sex) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$sexual_assault_per25, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, col.names = c(" ", "All Days", "Weekends", "Weekdays"),
      caption = "\\label{weekend_table_bootstrap}Effect of Moratoriums on Alcohol Offenses and Sexual Assault by Weekend/Weekdays (OLS).") %>% 
  kable_styling() %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = T, italic = F) %>% 
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = T, italic = F) %>% 
  footnote(list("95% onfidence intervals are obtained through a wild cluster bootstrap by university and each offense is defined as per-25000 enrolled students. The column All Days represents specification (2) from the main results table. Weekends consist of Fridays, Saturdays, and Sundays. Weekdays consist of Monday through Thursday. Holiday controls include controls for Veterans' Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since no university's academic-calendar contains them. A moratorium is a temporary halt on fraternity-related activities with alcohol.",
                "* p < 0.1, ** p < 0.05, *** p < 0.01"), threeparttable = T)
