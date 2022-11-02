
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

frac_ifc <- read_csv("data/fraction_ifc.csv")

moratoriums <- ifc::moratorium_lengths()
moratoriums <- moratoriums %>% 
  group_by(university) %>% 
  mutate(moratorium_id = n()) %>% 
  ungroup()
moratorium_summary <- moratoriums %>% 
  datasummary((`Number of Moratoriums per-University` = moratorium_id) +(`Length of Moratoriums` = length_moratorium) ~ (Mean + SD + Median + Min + Max ), data = .,
              output = "data.frame") 
moratorium_rows <- moratorium_summary %>%
  add_row(` ` = "Total Number of Universities",`Mean` =  "37",`SD` = " ",`Median` =  " ", `Min` = " ", `Max` =" ")


university_characteristics <- daily_crime %>%
  mutate(private = ifelse(control_of_institution != "Public", 1, 0)) %>% 
  datasummary((`Total Enrollment` = total_enrollment) + 
                (`Total Undergraduate Enrollment`=undergraduate_enrollment) + 
                (`Fraction Asian` = frac_undergrad_asian) +
                (`Fraction Black`= frac_undergrad_black) + 
                (`Fraction Hispanic` = frac_undergrad_hispanic_latino) +
                (`Fraction White` = frac_undergrad_white) +
                (`Graduation Rate` = graduation_rate_total_cohort) + 
                (`SAT Math 75th Percentile` = sat_math_75) +
                ( `SAT Reading 75th Percentile` = sat_reading_75) +
                (`Fraction Admitted` = frac_admitted_total) +
                (`Fraction Private`=private) +
                (`Alcohol Offense` = alcohol_offense_per25) +
                (`Sexual Assault` = sexual_assault_per25)  ~ (Mean + SD + Median + Min + Max ), data = .,
              add_rows = moratorium_rows , output = "data.frame") %>%
  mutate(across(-c(1), ~ifelse(row_number() == 1 | row_number() == 2, scales::comma(as.numeric(.)), .))) %>% 
  add_row(` ` = paste0("Fraction IFC Fraternity", footnote_marker_alphabet(1)),
          `Mean` =  sprintf("%.3f",mean(frac_ifc$ifc_frac_updated, na.rm = T)),
          `SD` = sprintf("%.3f",sd(frac_ifc$ifc_frac_updated, na.rm = T)),
          `Median` =  sprintf("%.3f",median(frac_ifc$ifc_frac_updated, na.rm = T)), 
          `Min` = sprintf("%.3f",min(frac_ifc$ifc_frac_updated, na.rm = T)), 
          `Max` =sprintf("%.3f",max(frac_ifc$ifc_frac_updated, na.rm = T)),
          .before = 12) %>% 
  mutate(across(-c(1), ~ifelse(row_number() == 1 | row_number() == 2, str_replace(., ".\\d\\d$", ""), .))) %>% 
  kbl(digits = 2, booktabs = T, 
      caption = "\\label{summary_stats}Summary Statistics of the Universities in the Sample",
      escape = F) %>% 
  row_spec(16, hline_after = T) %>% 
  row_spec(17, bold = F, italic = T) %>% 
  add_indent(c(2:6)) %>% 
  add_indent(c(3:6)) %>% 
  pack_rows("Panel A: University Characteristics", 1, 12, bold = T, italic = F) %>% 
  pack_rows("Panel B: Daily Crime Log Offenses", 13, 14, bold = T, italic = F, latex_gap_space = "0.5cm") %>% 
  pack_rows("Panel C: Moratorium Characteristics", 15, 16, bold = T, italic = F, latex_gap_space = "0.5cm") %>% 
  footnote("Offenses are per-25000 students enrolled per-academic calendar day. Length of moratorium statistics are in academic-calendar days. Number of moratoriums refers to number of moratoriums only within the 2014-2019 time period. Some schools may or may not have had moratoriums in periods before or after the time period of analysis. Only a subset of races were chosen, and hence, the fractions do not sum to 1 in the table. SAT Math 75th Percentile and SAT Reading 75th Percentile correspond to the 75th percentile SAT score for an admitted student. A perfect score is 800, while an average score is approximately 500. Fraction Private refers to the fraction of universities that are private universities.",
           threeparttable = T,
           alphabet = "The number of students in an IFC fraternity is based on the most recent number from 2014-2019. However, in the case of 4 univerisities, counts had to be obtained from year 2022 due to lack of data availability within departments. Note that IFC fraternity populations do not change substantially year-to-year.") %>% 
  kable_styling(latex_options = "HOLD_position", font_size = 11)
