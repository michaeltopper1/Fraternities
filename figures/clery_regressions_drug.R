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

clery %>% 
  colnames()
clery_regs_drug <- clery %>% 
  feols(c(clery_alcohol_per_25k, residencehall_liquor_per_25k, clery_oncampus_liquor_per_25k,
          clery_drug_per_25k,residencehall_drug_per_25k, clery_oncampus_drug_per_25k,
          alcohol_offense_per_25k, drug_offense_per_25k) ~treatment |
          university + year, cluster = ~university, data = .)
names(clery_regs_drug) = c("Full Sample", "Residence Hall", "On campus",
                      "Full Sample", "Residence Hall", "On campus",
                      "Full Sample", "Full Sample")

find_mean <- function(data, column) {
  column_mean <- data %>% 
    summarize(mean({{column}}, na.rm = T)) %>% 
    pull()
  return(column_mean)
}

add_means <- tribble(~term, ~alc_full, ~alc_res, ~alc_non, ~sex_full, ~sex_res, ~sex_non, ~dcl_alc, ~dcl_sex,
                     "Mean of Dependent Variable", find_mean(clery, clery_alcohol_per_25k), find_mean(clery, residencehall_liquor_per_25k), find_mean(clery,clery_oncampus_liquor_per_25k),
                     find_mean(clery,clery_drug_per_25k), find_mean(clery,residencehall_drug_per_25k ), find_mean(clery, clery_oncampus_drug_per_25k), find_mean(clery,alcohol_offense_per_25k), find_mean(clery, drug_offense_per_25k))

attr(add_means, 'position') <- c(4)

clery_reg_table_drug <- modelsummary(clery_regs_drug,
                                stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
                                add_rows = add_means,
                                coef_map = c("treatment" = "Moatorium"),
                                title = "\\label{clery_reg_drug}Effect of Moratoriums on Alcohol Offenses and Drug Offenses in Two Separate Data Sets.",
                                notes = list("Clery Act Data represents Campus Safety Report Official Statistics.",
                                             "Daily Crime Logs represent the novel data I constructed.",
                                             "Standard errors clustered by university.",
                                             "All data is aggregated at the calendar-year level.",
                                             "On campus crimes are all on-campus including residence hall crimes.",
                                             "Daily Crime Log data represents full calendar year- not only academic calendar days.")) %>% 
  add_header_above(c(" " = 1, "Alcohol Offenses" = 3, "Drug Offenses" = 3, "Alcohol Offenses" = 1,
                     "Drug Offenses" = 1)) %>% 
  add_header_above(c(" " = 1, "Campus Safety and Security" = 6, "Daily Crime Logs" = 2))


