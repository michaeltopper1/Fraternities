## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-02-21
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}

ifc_fraternities <- read_csv("data/ifc_fraternity_count.csv") %>% 
  filter(university %in% ifc::moratorium_schools())

missing_ifc_uni <- ifc_fraternities %>% 
  filter(is.na(most_recent)) %>% 
  pull(university)

frat_fraction <- daily_crime %>% 
  left_join(ifc_fraternities, by = c("university")) %>% 
  group_by(university) %>% 
  mutate(ifc_fraction = most_recent/undergraduate_enrollment) %>% 
  ungroup() %>%
  filter(!(university %in% missing_ifc_uni))


quantiles <- frat_fraction %>% 
  group_by(university) %>% 
  summarize(avg_ifc_fraction = mean(ifc_fraction)) %>% 
  pull(avg_ifc_fraction) %>% 
  quantile(c(.33, .66))

frat_fraction <- frat_fraction %>% 
  group_by(university) %>% 
  mutate(avg_ifc_frac = mean(ifc_fraction)) %>% 
  ungroup() %>% 
  mutate(ifc_frac_first_quant = ifelse(avg_ifc_frac <= quantiles[[1]], 1, 0)) %>% 
  mutate(ifc_frac_second_quant = ifelse(avg_ifc_frac >= quantiles[[1]] & avg_ifc_frac <= quantiles[[2]], 1, 0)) %>% 
  mutate(ifc_frac_third_quant = ifelse(avg_ifc_frac > quantiles[[2]], 1, 0))

frat_fraction_q1 <- frat_fraction %>% 
  filter(ifc_frac_first_quant == 1)
frat_fraction_q2 <- frat_fraction %>% 
  filter(ifc_frac_second_quant == 1)
frat_fraction_q3 <- frat_fraction %>% 
  filter(ifc_frac_third_quant == 1)

fe <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester")

outcomes <- c("alcohol_offense_per25",
              "sexual_assault_per25")


quantile_estimates_ifc_1 <- map(outcomes, ~ifc::reghdfe(frat_fraction , .,"treatment:ifc_frac_first_quant", fe, "university"))
quantile_estimates_ifc_2 <- map(outcomes, ~ifc::reghdfe(frat_fraction , .,"treatment:ifc_frac_second_quant", fe, "university"))
quantile_estimates_ifc_3 <- map(outcomes, ~ifc::reghdfe(frat_fraction , .,"treatment:ifc_frac_third_quant", fe, "university"))


ifc_table <- ifc::main_table(quantile_estimates_ifc_1,quantile_estimates_ifc_2, last_panel = quantile_estimates_ifc_3) %>% 
  slice(1:9) %>% 
  kbl(booktabs = T, col.names = c(" ", "Alcohol Offenses", "Sexual Assaults"),
      caption = "\\label{ifc_table}Effect of Moratoriums by Fraction of Enrollment in IFC") %>% 
  kable_styling(latex_options = "HOLD_position") %>% 
  pack_rows("Panel A: Below 33rd Percentile in IFC Participation", 1, 3) %>% 
  pack_rows("Panel B: Between 33rd and 66th Percentile in IFC Participation", 4, 6) %>% 
  pack_rows("Panel C: Above 66th Percentile in IFC Participation", 7, 9) %>% 
  add_header_above(c(" " = 1, "Type of Offense" = 2)) %>% 
  footnote(list("Standard errors are clustered by university and each offense is defined as per-25000 enrolled students. These estimates represent 34/38 universities with moratoriums. Four universities did not release information on their IFC populations. Each panel represents the interaction of an indicator equal to one if a university's fraction of IFC population to total enrollment was below the 33rd percentile, between the 33rd and 66th percentile, and above the 66th percentile. Controls include day of week, spring semester, holiday, and university by academic year. Holiday controls include controls for Veterans Day, Thanksgiving, Labor Day, Halloween, and MLK Day. Christmas/New Years/July 4th are not included since no university's academic calendar contains them. A moratorium is a temporary halt on fraternity-related activities with alcohol.",
                "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"),
           threeparttable = T) 
