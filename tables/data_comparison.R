
## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-28
##

library(tidyverse)
library(kableExtra)


data_used <- tibble(attributes = c("Source of Data:","Reporting Mandate:", "Level of Aggregation:", "Fraction Reporting Consistently:", "Alcohol Violations:", 
                                   "Sexual Assaults:", "Residence Hall Information:" ,"Analysis in Paper:"),
                    daily_crime_log = c("University police departments", "By-law", "Incident-level", "1.00", "All incidences reported to or by the university police.",
                                        "All incidences reported", "No", "Main analysis"),
                    clery = c("US Department of Education", "By-law", "Yearly", "1.00", "All incidences reported to or by any university entity.", "All incidences reported","Yes", "Substitution of partying"),
                    nibrs = c("FBI", "Voluntary", "Incident-level", as.character(round(9/38,2)), "Arrests only", "All incidences reported",
                               "No", "Spillovers of partying"),
                    ucr = c("FBI", "Voluntary", "Monthly", as.character(round(30/38, 2)), "None","Hierarchy rule",
                             "No", "Not used"))
data_used <-  kbl(data_used, booktabs = T, digits = 3,
    col.names = c(" ", "Daily Crime Logs", "Campus Safety and Security (CSS)", "National Incidence-Based Reporting System (NIBRS)", "Uniform Crime Reporting System (UCR)"),
    caption = "\\label{data_used}Comparison of All Relevant Data Sources") %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down")) %>%
  # add_header_above(c(" " = 1, "Main Analysis" = 1, "Secondary Analysis" = 1, "Unused" = 2)) %>% 
  add_header_above(c(" " = 1, "Data Source" = 4)) %>% 
  footnote(list("The Daily Crime Logs are used for the main analysis due to the advantages it has over the other sources. The fraction reporting consistently refers row corresponds to the fraction of the sample university police departments. For the NIBRS however, the fraction reported consistently refers to the number of university-specific and corresponding nearby police departments that report consistently. The hierarchy rule is a classification rule by the UCR where only the most serious crime in an incident is reported. While over 50 percent of UCR data is recorded to be reported consistently, the true percentage is difficult to know since NAs and 0s are treated the same in the data."),
                threeparttable = T) %>% 
  row_spec(8, hline_after = T) %>%
  pack_rows("Source and Requirement:", 1, 2, bold = T) %>% 
  pack_rows("Aggregation and Consistency:", 3, 4, bold = T) %>% 
  pack_rows("Offenses Reported and Location:", 5, 8,  bold = T) %>% 
  # column_spec(1, italic = F, bold = F, width = "6cm") %>% 
  # column_spec(2, italic = F, bold = F, width = "5cm") %>% 
  # column_spec(3, italic = F, bold = F, width = "5cm") %>% 
  # column_spec(4, italic = F, bold = F, width = "3cm") %>% 
  # column_spec(5, italic = F, bold = F, width = "3cm") %>% 
  landscape()

