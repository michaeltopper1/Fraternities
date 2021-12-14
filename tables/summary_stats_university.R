
## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-16
##

library(modelsummary)
library(tidyverse)
library(kableExtra)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv") %>% 
    filter(university %in% ifc::moratorium_schools())
}



moratoriums <- ifc::moratorium_lengths()
moratoriums <- moratoriums %>% 
  group_by(university) %>% 
  mutate(moratorium_id = n()) %>% 
  ungroup()
moratorium_summary <- moratoriums %>% 
  datasummary((`Number of Moratoriums Per-University` = moratorium_id) +(`Length of Moratoriums` = length_moratorium) ~ (Mean + SD + Median + Min + Max ), data = .,
              output = "data.frame") 
moratorium_rows <- moratorium_summary %>%
  add_row(` ` = "Total Number of Universities",`Mean` =  "38",`SD` = " ",`Median` =  " ", `Min` = " ", `Max` =" ")


university_characteristics <- daily_crime %>%
  mutate(private = ifelse(control_of_institution != "Public", 1, 0)) %>% 
  datasummary((`Total Enrollment` = total_enrollment) + 
                (`Total Undergrad Enrollment`=undergraduate_enrollment) + 
                (`Fraction Asian` = frac_undergrad_asian) +
                 (`Fraction Black`= frac_undergrad_black) + 
                (`Fraction Hispanic` = frac_undergrad_hispanic_latino) +
                (`Fraction White` = frac_undergrad_white) +
                 (`Graduation Rate` = graduation_rate_total_cohort) + 
                (`SAT Math 75` = sat_math_75) +
                ( `SAT Reading 75` = sat_reading_75) +
                (`Fraction Admitted` = frac_admitted_total) +
                (`Fraction Private`=private) +
                (`Alcohol Offense` = alcohol_offense_per25) +
                (`Drug Offense` = drug_offense_per25)  +
                (`Sexual Assault` = sexual_assault_per25)  ~ (Mean + SD + Median + Min + Max ), data = .,
              title = "\\label{summary_stats}Summary Statistics of the Universities in the Sample.",
              add_rows = moratorium_rows) %>% 
  row_spec(16, hline_after = T) %>% 
  row_spec(17, bold = F, italic = T) %>% 
  add_indent(c(2:6)) %>% 
  add_indent(c(3:6)) %>% 
  pack_rows("University Characteristics", 1, 11, bold = T, italic = F) %>% 
  pack_rows("Daily Crime Log Offenses", 12, 14, bold = T, italic = F) %>% 
  pack_rows("Moratorium Characteristics", 15, 16, bold = T, italic = F) %>% 
  footnote("Offenses are per-25000 students enrolled per-academic calendar day. Length of moratorium statistics are in academic calendar days. Number of moratoriums refers to number of moratoriums only within the 2014-2019 time period. Some schools may or may not have had moratoriums in periods before or after the time period of analysis. Only a subset of races were chosen, and hence, the sum of the fractions do not sum to 1 in the table.",
           threeparttable = T)

