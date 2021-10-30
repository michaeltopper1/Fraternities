
## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-28
##

library(tidyverse)
library(kableExtra)


data_used <- tibble(attributes = c("Source","Reporting Mandate", "Level of Aggregation", "Fraction of Sample Reporting Consistently", "Alcohol Violations", 
                                   "Sexual Assaults", "Drug Offenses", "Residence Hall Information" ,"Analysis in Paper"),
                    daily_crime_log = c(" Clery Act Requests", "By-law", "Incident-level", "1", "All Incidences Reported",
                                        "All Incidences Reported","All Incidences Reported", "No", "Main Analysis"),
                    clery = c("US Department of Education", "By-law", "Yearly", "1", "All Incidences Reported", "All Incidences",
                              "All Incidences Reported","Yes", "Secondary"),
                    nibrs = c("FBI", "Voluntary", "Incident-level", as.character(round(14/38,3)), "Arrests Only", "All Incidences Reported",
                              "All Incidences Reported", "No", "Not Used"),
                    ucr = c("FBI", "Voluntary", "Monthly", as.character(round(30/38, 3)), "None","Hierarchy Rule",
                            "None", "No", "Not Used"))
kbl(data_used, booktabs = T, digits = 3,
    col.names = c("Characteristics", "Daily Crime Logs", "Campus Safety and Security", "NIBRS", "UCR")) %>%
  kable_styling() %>%
  # add_header_above(c(" " = 1, "Main Analysis" = 1, "Secondary Analysis" = 1, "Unused" = 2)) %>% 
  add_header_above(c(" " = 1, "Data Source" = 4)) %>% 
  footnote(list("Hierarchy Rule is where only the most serious crime in an incident is reported.",
                "While over 50 percent of UCR data is displayed to be reported consistently, it is actually truly unknown since NAs and 0s are the same.")) %>% 
  row_spec(8, hline_after = T) %>%
  column_spec(1, italic = T, width = "6cm") %>% 
  landscape()
