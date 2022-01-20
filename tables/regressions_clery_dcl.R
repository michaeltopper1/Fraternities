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

## changing to per 25000 enrolled students per day. 
clery <- clery %>% 
  mutate(across(ends_with("per25"), ~ . / 365))

explanatory_vars <- c("treatment")
fixed_effects <- c("year", "university")

alc_offenses <- list("alcohol_offense_per25", "clery_alcohol_per25", "residencehall_liquor_per25")

sex_offenses <- list("sexual_assault_per25", "clery_sexual_assault_per25", "residencehall_sexual_assault_per25")

alc_clery <- map(alc_offenses, ~ifc::reghdfe(clery, ., explanatory_vars, fixed_effects, cluster = "university"))
sex_clery <- map(sex_offenses, ~ifc::reghdfe(clery, ., explanatory_vars, fixed_effects, cluster = "university"))

clery_compare <- ifc::main_table(alc_clery, last_panel = sex_clery) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(clery$alcohol_offense_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(clery$clery_alcohol_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(clery$residencehall_liquor_per25, na.rm = T)),
          .before = 4) %>% 
  add_row(term = "Mean of Dependent Variable", 
          `Model 1` = sprintf("%.3f",mean(clery$sexual_assault_per25, na.rm = T)),
          `Model 2` = sprintf("%.3f",mean(clery$clery_sexual_assault_per25, na.rm = T)),
          `Model 3` = sprintf("%.3f",mean(clery$residencehall_sexual_assault_per25, na.rm = T)),
          .before = 8) %>% 
  kbl(booktabs = T, 
      col.names = c(" ", "Full Sample", "Full Sample", "Residence Halls"),
      digits = 3,
      caption = "\\label{clery_compare}Effect of Moratoriums on Alcohol Offenses, Drug Offenses, and Sexual Assaults: Comparison of Daily Crime Logs and Campus Safety and Security (OLS).") %>% 
  kable_paper() %>% 
  pack_rows("Panel A: Alcohol Offenses", 1, 4, bold = T, italic = F) %>%
  pack_rows("Panel B: Sexual Assaults", 5, 8, bold = T, italic = F) %>% 
  pack_rows("Controls for Panels A-B:", 9, 10, bold = T, italic = F) %>% 
  add_header_above(c(" " = 1, "Daily Crime Logs" = 1, 
                     "Campus Safety and Security" = 2)) %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as offense per-25000 enrolled students per-calendar day. Recall that Daily Crime Logs are the primary source of data used in prior analysis. In this model, the 'In Moratorium' treatment variable is defined as a fraction between 0 and 1 where the fraction represents the proportion of calendar-days that experienced a moratorium in a calendar year. Full Samples include the entire Daily Crime Logs/Campus Safety and Security Data (CSS), while Residence Halls is a subset of the CSS. Full Sample in the CSS data contains both off-campus and on-campus reports. CSS data does not necessary need to be reported to the university police and hence, may not show up in the Daily Crime Logs.  A moratorium is a temporary halt on fraternity-related activities with alcohol.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"),
           threeparttable = T)