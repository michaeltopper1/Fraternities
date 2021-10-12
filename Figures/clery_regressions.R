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

clery <- read_csv(here::here("Created Data/xMaster_data_2021/merged_clery.csv")) %>% 
  filter(university %in% ifc::moratorium_schools())

clery %>% colnames()
clery_regs <- clery %>% 
  feols(c(clery_alcohol_per_25k, residencehall_liquor_per_25k, noncampus_liquor_per_25k,
          clery_sexual_assault_per_25k,residencehall_sexual_assault_per_25k, clery_offcampus_sexual_assault_per_25k,
          alcohol_offense_per_25k, sexual_assault_per_25k) ~treatment |
          university + year, cluster = ~university, data = .)
names(clery_regs) = c("Full Sample", "Residence Hall", "Noncampus",
                   "Full Sample", "Residence Hall", "Noncampus",
                   "Full Sample", "Full Sample")

find_mean <- function(data, column) {
  column_mean <- data %>% 
    summarize(mean({{column}}, na.rm = T)) %>% 
    pull()
  return(column_mean)
}

add_means <- tribble(~term, ~alc_full, ~alc_res, ~alc_non, ~sex_full, ~sex_res, ~sex_non, ~dcl_alc, ~dcl_sex,
                     "Mean of Dependent Variable", find_mean(clery, clery_alcohol_per_25k), find_mean(clery, residencehall_liquor_per_25k), find_mean(clery,noncampus_liquor_per_25k),
                     find_mean(clery,clery_sexual_assault_per_25k), find_mean(clery,residencehall_sexual_assault_per_25k ), find_mean(clery, clery_offcampus_sexual_assault_per_25k), find_mean(clery,alcohol_offense_per_25k), find_mean(clery, sexual_assault_per_25k))

attr(add_means, 'position') <- c(4)

clery_reg_table <- modelsummary(clery_regs,
             stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
             add_rows = add_means,
             coef_map = c("treatment" = "Moatorium"),
             title = "\\label{clery_reg}Effect of Moratoriums on Alcohol Offenses and Sexual Assaults in Two Separate Data Sets.",
             notes = list("Clery Act Data represents Campus Safety Report Official Statistics.",
                          "Daily Crime Logs represent the novel data I constructed.",
                          "Standard errors clustered by university.",
                          "All data is aggregated at the calendar-year level.",
                          "Daily Crime Log data represents full calendar year- not only academic calendar days.")) %>% 
  add_header_above(c(" " = 1, "Alcohol Offenses" = 3, "Sexual Assault" = 3, "Alcohol Offenses" = 1,
                     "Sexual Assault" = 1)) %>% 
  add_header_above(c(" " = 1, "Campus Safety and Security" = 6, "Daily Crime Logs" = 2))


