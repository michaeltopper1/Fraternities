
## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-28
##

library(tidyverse)

compare_data <- tibble(data = c("Daily Crime Logs", "Campus Safety and Security", "NIBRS", "UCR"),
       source = c("Clery Act Requests", "US Department of Education", "FBI", "FBI"),
       reporting = c("By-law", "By-law", "Voluntary", "Voluntary"),
       aggregation_level = c("Incident-level", "Yearly", "Incident-level", "Monthly"),
       fraction_of_schools = c(1, 1, 14/38, 30/38),
       alcohol_violations = c("All Incidences", "All Incidences", "Arrests Only", "None"),
       sexual_assaults = c("All Incidences", "All Incidences", "All Incidences", "Hierarchy Rule"),
       drug_violations = c("All Incidences", "All Incidences", "All Incidences", "None"),
       residence_hall = c("No", "Yes", "No", "No"),
       used_in_analysis = c("Main Analysis", "Secondary", "Not Used", "Not Used"))
kbl(compare_data, booktabs = T, digits = 3,
    col.names = c("  ", "Source","Reporting", "Level of Aggregation", "Fraction of Sample Reporting Consistently", "Alcohol Violations", 
                  "Sexual Assaults", "Drug Offenses", "Residence Hall Information" ,"Analysis")) %>% 
  kable_styling() %>% 
  add_header_above(c(" " = 1, "Data Comparison Table" = 9)) %>% 
  footnote(list("Hierarchy Rule is where only the most serious crime in an incident is reported.",
                "While over 50 percent of UCR data is displayed to be reported consistently, it is actually truly unknown since NAs and 0s are the same."))
