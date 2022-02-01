## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-07
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(gt)

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
  gt() %>% 
  cols_label(term = " ", `Model 1` = "Full Sample", `Model 2` = "Full Sample", `Model 3` = "Residence Halls") %>% 
  tab_header(title = "Effect of Moratoriums on Alcohol Offenses and Sexual Assaults: Comparison of Daily Crime Logs and Campus Safety and Security (OLS).") %>%
  tab_row_group(label = "Panel B: Sexual Assaults", rows = c(5:8)) %>% 
  tab_row_group(label = "Panel A: Alcohol Offenses", rows = c(1:4)) %>% 
  tab_spanner(label = "Daily Crime Logs", columns = 2) %>% 
  tab_spanner(label = "Campus Safety and Security", columns = c(3:4)) %>% 
  tab_source_note("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001") %>% 
  tab_style(style = cell_text(weight = "bold"), locations = cells_row_groups()) %>% 
  tab_options(table.font.size = pct(50))
  