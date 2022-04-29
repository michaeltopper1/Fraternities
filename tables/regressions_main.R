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

## for bootstrapping
# set seed via dqset.seed for boot_algo = "R" & Rademacher, Webb & Normal weights
dqrng::dqset.seed(2352342)
# set 'familiar' seed for all other algorithms and weight types 
set.seed(23325)

explanatory_vars <- c("treatment")


# fixed effects for daily_level -------------------------------------------
fixed_effects_1 <- c("day_of_week", "academic_year", "spring_semester", "university", "holiday", "game_occurred")
fixed_effects_2 <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")
fixed_effects_3 <- c("day_of_week", "university_by_academic_year_by_semester", "holiday", "game_occurred")


daily_fixed_effects = list(fixed_effects_1, fixed_effects_2, fixed_effects_3)

alc <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)

alc_boot <- map(alc, ~boottest(., param = "treatment", clustid = "university", B = 1000))


sex <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("sexual_assault_per25"),explanatory_vars, ., "university")
)

sex_boot <- map(sex, ~boottest(., param = "treatment", clustid = "university", B = 1000))


main_table <- ifc::main_table(alc, last_panel = sex) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Wild Bootstrap P-Value", 
          `Model 1` = sprintf("%.3f",alc_boot[[1]]$p_val),
          `Model 2` = sprintf("%.3f",alc_boot[[2]]$p_val),
          `Model 3` = sprintf("%.3f",alc_boot[[3]]$p_val),
          .before = 5) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          .before = 9) %>% 
  add_row(term = "Wild Bootstrap P-Value", 
          `Model 1` = sprintf("%.3f",sex_boot[[1]]$p_val),
          `Model 2` = sprintf("%.3f",sex_boot[[2]]$p_val),
          `Model 3` = sprintf("%.3f",sex_boot[[3]]$p_val),
          .before = 10) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)"),
      digits = 3,
      caption = "\\label{main_table}Effect of Moratoriums on Alcohol Offenses and Sexual Assaults (OLS).") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 5, bold = F, italic = T) %>%
  pack_rows("Panel B: Sexual Assaults", 6, 10, bold = F, italic = T) %>% 
  pack_rows("Controls for Panels A-B:",11, 18, bold = F, italic = T ) %>% 
  # row_spec(5, italic = T) %>% 
  footnote(list("Estimates are obtained using OLS. Standard errors shown in paranthesis are clustered by university (37 clusters) and each offense is defined as per-25000 enrolled students. P-values from 1000 wild cluster bootstrap iterations are shown for the In Moratorium coefficient as suggested by @cameron_bootstrap-based_2008 in cases with a small number of clusters (typically lower than 30). This analysis is near, but not below this threshold. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since these holiday's are not on any university's academic calendar. Game Day controls consist of university football games within each university. A moratorium is a temporary halt on fraternity-related activities with alcohol. Specification (2) is the preferred specification due to the flexibility of the fixed effects and the conservativeness of the estimates.",
                "* p < 0.1, ** p < 0.05, *** p < 0.01"), threeparttable = T)


# table 2: weekends vs. full sample ---------------------------------------

fixed_effects_preferred <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")
# fixed_effects_preferred_l <-  c("day_of_week", "university_by_academic_year_by_semester", "holiday")
data_subsets <- list(daily_crime, daily_crime_weekends, daily_crime_weekdays)

alc_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("alcohol_offense_per25"),explanatory_vars, fixed_effects_preferred, "university")
)

sex_weeksplit <- map(data_subsets, ~ifc::reghdfe(., c("sexual_assault_per25"),explanatory_vars, fixed_effects_preferred, "university")
)



# getting bootstrapped regressions ----------------------------------------

w_alc_2 <- feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                 data = daily_crime_weekends, cluster = "university")

wboot_alc_2 <- boottest(w_alc_2, clustid = "university", param = "treatment", B = 1000)

w_alc_3 <- feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                 data = daily_crime_weekdays, cluster = "university")

wboot_alc_3 <- boottest(w_alc_3, clustid = "university", param = "treatment", B = 1000)


# bootstrapped regressions for alcohol ------------------------------------


w_sex_2 <- feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                 data = daily_crime_weekends, cluster = "university")

wboot_sex_2 <- boottest(w_sex_2, clustid = "university", param = "treatment", B = 1000)

w_sex_3 <- feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_academic_year + game_occurred + holiday + spring_semester,
                 data = daily_crime_weekdays, cluster = "university")

wboot_sex_3 <- boottest(w_sex_3, clustid = "university", param = "treatment", B = 1000)






weekend_table <- ifc::main_table(alc_weeksplit, last_panel = sex_weeksplit) %>% 
  slice(1:6) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Wild Bootstrap P-Value", 
          `Model 1` = sprintf("%.3f",alc_boot[[2]]$p_val),
          `Model 2` = sprintf("%.3f",wboot_alc_2$p_val),
          `Model 3` = sprintf("%.3f",wboot_alc_3$p_val),
          .before = 5) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime_weekends$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime_weekdays$sexual_assault_per25, na.rm = T)),
          .before = 9) %>% 
  add_row(term = "Wild Bootstrap P-Value", 
          `Model 1` = sprintf("%.3f",sex_boot[[2]]$p_val),
          `Model 2` = sprintf("%.3f",wboot_sex_2$p_val),
          `Model 3` = sprintf("%.3f",wboot_sex_3$p_val),
          .before = 10) %>% 
  kbl(booktabs = T, col.names = c(" ", "All Days", "Weekends", "Weekdays"),
      caption = "\\label{weekend_table}Effect of Moratoriums on Alcohol Offenses and Sexual Assault by Weekend/Weekdays (OLS).") %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 5, bold = T, italic = F) %>% 
  pack_rows("Panel B: Sexual Assaults", 6, 10, bold = T, italic = F) %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  add_header_above(c(" " = 1, "Days of the Week" = 3)) %>% 
  column_spec(1, width = "8cm") %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as per-25000 enrolled students. The column All Days represents specification (2) from the main results table. Weekends consist of Fridays, Saturdays, and Sundays. Weekdays consist of Monday through Thursday. Holiday controls include controls for Veterans' Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since no university's academic-calendar contains them. A moratorium is a temporary halt on fraternity-related activities with alcohol.",
                    "* p < 0.1, ** p < 0.05, *** p < 0.01"),
           threeparttable = T) 

