## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-10-24
##
## Note: this table needs to be done brute-force due to the shittiness of fwildclusterboot v0.7

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


# creating fe columns for day of week to make it work with fboot ----------
daily_crime <- daily_crime %>% 
  mutate(day_of_week_fe = lubridate::wday(date)) 

daily_crime_weekends <- daily_crime_weekends %>% 
  mutate(day_of_week_fe = lubridate::wday(date)) 

daily_crime_weekdays <- daily_crime_weekdays %>% 
  mutate(day_of_week_fe = lubridate::wday(date)) 

## for bootstrapping
# set seed via dqset.seed for boot_algo = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(2352342)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(23325)

explanatory_vars <- c("treatment")


# fixed effects for daily_level -------------------------------------------
fixed_effects_1 <- c("day_of_week_fe", "academic_year", "spring_semester", "university", "holiday", "game_occurred")
fixed_effects_2 <- c("day_of_week_fe", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")
fixed_effects_3 <- c("day_of_week_fe", "university_by_academic_year_by_semester", "holiday", "game_occurred")

alc_1 <- feols(alcohol_offense_per25 ~ treatment | 
                 day_of_week_fe + academic_year + university + 
                 holiday + spring_semester + game_occurred,
               cluster = "university", 
               data = daily_crime)

alc_2 <- feols(alcohol_offense_per25 ~ treatment | 
                 day_of_week_fe + university_by_academic_year + 
                 holiday + spring_semester + game_occurred,
               cluster = "university", 
               data = daily_crime)

alc_3 <- feols(alcohol_offense_per25 ~ treatment | 
                 day_of_week_fe + university_by_academic_year_by_semester + 
                 holiday + game_occurred,
               cluster = "university", 
               data = daily_crime)

## column 4 weekends preferred
alc_4_weekend <- feols(alcohol_offense_per25 ~ treatment | 
                         day_of_week_fe + university_by_academic_year + 
                         holiday + spring_semester + game_occurred,
                       cluster = "university", 
                       data = daily_crime_weekends)

## column 5 weekdays preferred
alc_5_weekday <- feols(alcohol_offense_per25 ~ treatment | 
                         day_of_week_fe + university_by_academic_year + 
                         holiday + spring_semester + game_occurred,
                       cluster = "university", 
                       data = daily_crime_weekdays)



alc_main <- list(alc_1, alc_2, alc_3, alc_4_weekend, 
                 alc_5_weekday)

alc_boot <- map(alc_main, ~boottest(., param = "treatment", clustid = "university", B = 1000))

alc_boot_pvalues <- map(alc_boot, ~.x$p_val) %>% 
  unlist()


sex_1 <- feols(sexual_assault_per25 ~ treatment | 
                 day_of_week_fe + academic_year + university + 
                 holiday + spring_semester + game_occurred,
               cluster = "university", 
               data = daily_crime)

sex_2 <- feols(sexual_assault_per25 ~ treatment | 
                 day_of_week_fe + university_by_academic_year + 
                 holiday + spring_semester + game_occurred,
               cluster = "university", 
               data = daily_crime)


sex_3 <- feols(sexual_assault_per25 ~ treatment | 
                 day_of_week_fe + university_by_academic_year_by_semester + 
                 holiday + game_occurred,
               cluster = "university", 
               data = daily_crime)

## column 4 weekends preferred
sex_4_weekend <- feols(sexual_assault_per25 ~ treatment | 
                         day_of_week_fe + university_by_academic_year + 
                         holiday + spring_semester + game_occurred,
                       cluster = "university", 
                       data = daily_crime_weekends)

## column 5 weekdays preferred
sex_5_weekday <- feols(sexual_assault_per25 ~ treatment | 
                         day_of_week_fe + university_by_academic_year + 
                         holiday + spring_semester + game_occurred,
                       cluster = "university", 
                       data = daily_crime_weekdays)


sex_main <- list(sex_1, sex_2, sex_3, sex_4_weekend, sex_5_weekday)

sex_boot <- map(sex_main, 
                ~boottest(., param = "treatment", clustid = "university", B = 1000))

sex_boot_pvalues <- map(sex_boot,
                        ~.x$p_val) %>% 
  unlist()

# table 2: weekends vs. full sample ---------------------------------------


main_table <- ifc::main_table(alc_main, last_panel = sex_main) %>%
  mutate(term = ifelse(row_number() == 2 | row_number() == 5, "", term)) %>% 
  add_row(term = "Mean of Dependent Variable",
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(daily_crime_weekends$alcohol_offense_per25, na.rm = T)),
          `Model 5` = sprintf("%.3f",mean(daily_crime_weekdays$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>%
  add_row(term = "Wild Bootstrap P-Value",
          `Model 1` = sprintf("%.3f",alc_boot_pvalues[[1]]),
          `Model 2` = sprintf("%.3f",alc_boot_pvalues[[2]]),
          `Model 3` = sprintf("%.3f",alc_boot_pvalues[[3]]),
          `Model 4` = sprintf("%.3f",alc_boot_pvalues[[4]]),
          `Model 5` = sprintf("%.3f",alc_boot_pvalues[[5]]),
          .before = 5) %>%
  add_row(term = "Mean of Dependent Variable",
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 4` = sprintf("%.3f",mean(daily_crime_weekends$sexual_assault_per25, na.rm = T)),
          `Model 5` = sprintf("%.3f",mean(daily_crime_weekdays$sexual_assault_per25, na.rm = T)),
          .before = 9) %>%
  add_row(term = "Wild Bootstrap P-Value",
          `Model 1` = sprintf("%.3f",sex_boot_pvalues[[1]]),
          `Model 2` = sprintf("%.3f",sex_boot_pvalues[[2]]),
          `Model 3` = sprintf("%.3f",sex_boot_pvalues[[3]]),
          `Model 4` = sprintf("%.3f",sex_boot_pvalues[[4]]),
          `Model 5` = sprintf("%.3f",sex_boot_pvalues[[5]]),
          .before = 10) %>%
  kbl(booktabs = T,
      col.names = c(" ", "(1)", "(2)", "(3)", "(4)", "(5)"),
      digits = 3,
      caption = "\\label{main_table}Effect of Moratoriums on Alcohol Offenses and Sexual Assaults (OLS)", align = 'lccccc') %>%
  kable_styling(latex_options = "HOLD_position", font_size = 11) %>%
  pack_rows("Panel A: Alcohol Offenses", 1, 5, bold = F, italic = T) %>%
  pack_rows("Panel B: Sexual Assaults", 6, 10, bold = F, italic = T, latex_gap_space = "0.5cm") %>%
  add_header_above(c(" " = 4, "Weekends" = 1, "Weekdays" = 1), line = F) %>%
  add_header_above(c(" " = 4, "Specification (2)" = 2)) %>%
  row_spec(c(10),hline_after=TRUE) %>%
  # pack_rows("",11, 18, bold = F, italic = T, hline_before = T ) %>%
  # row_spec(5, italic = T) %>%
  column_spec(1, width = "8cm") %>%
  row_spec(c(18), hline_after =T) %>%
  footnote(list("Estimates are obtained using OLS. Standard errors shown in parenthesis are clustered by university (37 clusters) and each offense is defined as per-25000 enrolled students. P-values from 1000 wild cluster bootstrap iterations are shown for the In Moratorium coefficient as suggested by Cameron, Gelbach, and Miller (2008) in cases with a small number of clusters (typically lower than 30). This analysis is near, but not below this threshold. Game Day controls consist of university football games within each university. Weekends include Friday-Sunday while Weekdays include Monday-Thursday. Column 2 is the preferred specification due to the flexibility of the fixed effects and the conservativeness of the estimates. Significance stars correspond to clustered standard errors.",
                "* p < 0.1, ** p < 0.05, *** p < 0.01"), threeparttable = T)



