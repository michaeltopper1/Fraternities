## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-07
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

clery <- read_csv(here::here("created_data/xmaster_data/merged_clery.csv")) %>% 
  filter(university %in% ifc::moratorium_schools())

clery <- clery %>% 
  mutate(treatment = treatment_day)


explanatory_vars <- c("treatment")
fixed_effects <- c("year", "university")

alc_offenses <- list("alcohol_offense_per25", "clery_alcohol_per25", "residencehall_liquor_per25", "liquor_total_arrests_per25", "liquor_residencehallarrest_per25")

sex_offenses <- list("sexual_assault_per25", "clery_sexual_assault_per25", "residencehall_sexual_assault_per25")

alc_clery <- map(alc_offenses, ~ifc::reghdfe(clery, ., explanatory_vars, fixed_effects, cluster = "university"))
alc_clery_weighted <- map(alc_offenses, ~ifc::reghdfe(clery, ., explanatory_vars, fixed_effects, cluster = "university",
                                             weights = clery$total_enrollment))

sex_clery <- map(sex_offenses, ~ifc::reghdfe(clery, ., explanatory_vars, fixed_effects, cluster = "university"))

gm <- tibble::tribble(
  ~raw,      ~clean,          ~fmt,  ~omit,
  "nobs", "Observations", 0,  FALSE,
  "mean", "Mean of Dependent Variable", 3, FALSE,
  "FE: year", "FE: Year", 0, FALSE,
  "FE: university", "FE: University", 0, FALSE
)

note_css <- "Standard errors are clustered by university and each offense is defined as offense per-25000 enrolled students per-calendar year. Recall that Daily Crime Logs are the primary source of data used in prior analysis. In this model, the In Moratorium treatment variable is defined as the number of calendar-days that experienced a moratorium in a calendar-year. All Reports columns include the entire Daily Crime Logs/Campus Safety and Security Data (CSS), while Residence Halls is a subset of the CSS. All Reports in the CSS data contains both off-campus and on-campus reports. CSS data does not necessarily need to be reported to the university police and hence, may not show up in the Daily Crime Logs. Columns 2 and 3 refer to disciplinary actions for liquor law violations and reported crime for sexual assaults. Columns 4 and 5 refer to arrests for liquor law violations. Fixed effects include university and year fixed effects"
css_arrests <- panelsummary::panelsummary(alc_clery,sex_clery,
                           mean_dependent = T,
                           stars = c('*' = .1, '**' = .05, '***' = .01), 
                           panel_labels = c("Panel A: Alcohol Offenses",
                                            "Panel B: Sexual Assaults"),
                           coef_map = c("treatment" = "In Moratorium"),
                           gof_map = gm,
                           collapse_fe = F,
                           hline_before_fe = T,
                           italic = T,
                           bold = F,
                           caption = "\\label{css_arrests}Effect of Moratoriums on Alcohol Offenses and Sexual Assaults: Comparison of Daily Crime Logs and Campus Safety and Security (OLS)") %>% 
  add_header_above(c(" " = 1, "All Reports" = 1, 
                     "All Reports" = 1, "Residence Halls" = 1,
                     "All Reports" = 1, "Residence Halls")) %>% 
  add_header_above(c(" " = 1, "Daily Crime Logs" = 1, "Displinary Actions/Reported Crime" = 2, " Arrests" = 2)) %>% 
  add_header_above(c(" " = 1, " " = 1, 
                     "Campus Safety and Security" = 4)) %>% 
  footnote(list(note_css,
                "* p < 0.1, ** p < 0.05, *** p < 0.01"), threeparttable = T)
  

# 
# clery_compare <- ifc::main_table(alc_clery, last_panel = sex_clery) %>%
#   add_row(term = "Mean of Dependent Variable",
#           `Model 1` = sprintf("%.3f",mean(clery$alcohol_offense_per25, na.rm = T)),
#           `Model 2` = sprintf("%.3f",mean(clery$clery_alcohol_per25, na.rm = T)),
#           `Model 3` = sprintf("%.3f",mean(clery$residencehall_liquor_per25, na.rm = T)),
#           `Model 4` = sprintf("%.3f",mean(clery$liquor_total_arrests_per25, na.rm = T)),
#           `Model 5` = sprintf("%.3f",mean(clery$liquor_residencehallarrest_per25, na.rm = T)),
#           .before = 4) %>%
#   add_row(term = "Mean of Dependent Variable",
#           `Model 1` = sprintf("%.3f",mean(clery$sexual_assault_per25, na.rm = T)),
#           `Model 2` = sprintf("%.3f",mean(clery$clery_sexual_assault_per25, na.rm = T)),
#           `Model 3` = sprintf("%.3f",mean(clery$residencehall_sexual_assault_per25, na.rm = T)),
#           `Model 4` = " ",
#           `Model 5` = " ",
#           .before = 8) %>%
#   kbl(booktabs = T,
#       col.names = c(" ", "(1)", "(2)", "(3)", "(4)", "(5)"), align = "lccccc",
#       digits = 4,
#       caption = "\\label{clery_compare}Effect of Moratoriums on Alcohol Offenses and Sexual Assaults: Comparison of Daily Crime Logs and Campus Safety and Security (OLS)") %>%
#   kable_paper() %>%
#   pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = F, italic = T) %>%
#   pack_rows("Panel B: Sexual Assaults", 5, 8, bold = F, italic = T, latex_gap_space = "0.5cm") %>%
#   row_spec(c(8),hline_after=TRUE) %>%
#   add_header_above(c(" " = 1, "All Reports" = 1, 
#                      "All Reports" = 1, "Residence Halls" = 1,
#                      "All Reports" = 1, "Residence Halls")) %>% 
#   add_header_above(c(" " = 1, "Daily Crime Logs" = 1, "Displinary Actions/Reported Crime" = 2, " Arrests" = 2)) %>% 
#   add_header_above(c(" " = 1, " " = 1, 
#                      "Campus Safety and Security" = 4))%>%
#   column_spec(1, width = "8cm") %>%
#   row_spec(10, hline_after = T) %>%
#   footnote(list(note_css,
#                 "* p < 0.1, ** p < 0.05, *** p < 0.01"),
#            threeparttable = T)
# 
# 
# 
# 
# 
