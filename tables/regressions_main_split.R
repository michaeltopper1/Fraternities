## Purpose of script: split data into two section to investigate fixed effects.
##
## Author: Michael Topper
##
## Date Last Edited: 2021-12-06
##

library(tidyverse)
library(modelsummary)
library(fixest)
library(kableExtra)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv")
}


short_moratorium_schools <- 
  c("University of California-Berkeley",
    "East Carolina University",
    "Clemson University" ,
    "University of Virginia-Main Campus",
    "Emory University",
    "Arkansas State University-Main Campus",
    "University of Michigan-Ann Arbor",
    "University of Kansas",
    "University of Missouri-Columbia",
    "Marshall University",
    "Ohio University-Main Campus",
    "Syracuse University",
    "Rutgers University-New Brunswick",
    "Tufts University",
    "Murray State University",
    "Florida International University",
    "San Diego State University",
    "Washington State University",
    "University at Buffalo" )
short_moratorium_schools

short_moratorium_schools_fullsem <- c("University of California-Berkeley",
                                      "East Carolina University",
                                      "Clemson University" ,
                                      "Emory University",
                                      "Arkansas State University-Main Campus",
                                      "University of Kansas",
                                      "University of Missouri-Columbia",
                                      "Marshall University",
                                      "Ohio University-Main Campus",
                                      "Syracuse University",
                                      "Rutgers University-New Brunswick",
                                      "Florida International University",
                                      "San Diego State University",
                                      "Washington State University")

daily_crime_short <- daily_crime %>% 
  filter((university %in% short_moratorium_schools))

daily_crime_long <- daily_crime %>% 
  filter(!(university %in% short_moratorium_schools))

explanatory_vars <- c("treatment")
fixed_effects_0 <- c("day_of_week", "academic_year", "spring_semester", "university", "holiday")
fixed_effects_1 <- c( "day_of_week", "university_by_academic_year", "holiday", "spring_semester")
fixed_effects_2 <- c("day_of_week", "university_by_academic_year_by_semester", "holiday", "spring_semester")



daily_fixed_effects = list(fixed_effects_0,fixed_effects_1, fixed_effects_2)



# long moratorium schools regs --------------------------------------------


alc_long <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime_long, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)

drug_long <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime_long, c("drug_offense_per25"),explanatory_vars, ., "university")
)

sex_long <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime_long, c("sexual_assault_per25"),explanatory_vars, ., "university")
)


# short moratorium schools regs -------------------------------------------


alc_short <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime_short, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)

drug_short <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime_short, c("drug_offense_per25"),explanatory_vars, ., "university")
)

sex_short <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime_short, c("sexual_assault_per25"),explanatory_vars, ., "university")
)

table_differences <- ifc::main_table(alc_long, drug_long, last_panel =  sex_long) %>% 
  bind_cols(ifc::main_table(alc_short, drug_short, last_panel = sex_short)) %>% 
  select(-`term...5`) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)", "(1)", "(2)", "(3)"),
      digits = 3,
      caption = "\\label{table_differences}Effect of Moratoriums on Alcohol Offenses, Drug Offenses, and Sexual Assault by Moratorium Length.") %>% 
  kable_paper() %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 3, bold = F, italic = T) %>%
  pack_rows("Panel B: Drug Offenses", 4, 6, bold = F, italic = T) %>%
  pack_rows("Panel C: Sexual Assaults", 7, 9, bold = F, italic = T) %>% 
  pack_rows("Controls for Panels A-C:",10, 15, bold = F, italic = T ) %>% 
  footnote(list("Standard errors are clustered by university.",
                "Offenses are per-25000 enrolled students.",
                "Moratorium is a temporary halt on fraternity-related activities with alcohol.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001")) %>% 
  add_header_above(c(" " = 1, "Greater than 48-day Moratorium" = 3, "Less than 48-day Moratorium" = 3))


alc <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("alcohol_offense_per25"),explanatory_vars, ., "university")
)

drug <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("drug_offense_per25"),explanatory_vars, ., "university")
)

sex <- map(daily_fixed_effects, ~ifc::reghdfe(daily_crime, c("sexual_assault_per25"),explanatory_vars, ., "university")
)



main_table <- ifc::main_table(alc, drug, last_panel = sex) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$alcohol_offense_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$drug_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$drug_offense_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$drug_offense_per25, na.rm = T)),
          .before = 8) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(daily_crime$sexual_assault_per25, na.rm = T)),
          .before = 12) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "(1)", "(2)", "(3)"),
      digits = 3,
      caption = "\\label{main_table}Effect of Moratoriums on Alcohol Offenses, Drug Offenses, and Sexual Assault.") %>% 
  kable_paper() %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = F, italic = T) %>%
  pack_rows("Panel B: Drug Offenses", 5, 8, bold = F, italic = T) %>%
  pack_rows("Panel C: Sexual Assaults", 9, 12, bold = F, italic = T) %>% 
  pack_rows("Controls for Panels A-C:",13, 19, bold = F, italic = T ) %>% 
  footnote(list("Standard errors are clustered by university.",
                "Offenses are per-25000 enrolled students.",
                "Moratorium is a temporary halt on fraternity-related activities with alcohol.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"))

