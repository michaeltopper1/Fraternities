
## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-28
##

library(tidyverse)
library(gt)


data_used <- tibble(attributes = c("Source of Data:","Reporting Mandate:", "Level of Aggregation:", "Fraction Reporting Consistently:", "Alcohol Violations:", 
                                   "Sexual Assaults:", "Residence Hall Information:" ,"Analysis in Paper:"),
                    daily_crime_log = c("University Police Departments", "By-law", "Incident-level", "1.000", "All Incidences Reported",
                                        "All Incidences Reported", "No", "Main Analysis"),
                    clery = c("US Department of Education", "By-law", "Yearly", "1.000", "All Incidences Reported", "All Incidences Reported","Yes", "Secondary"),
                    nibrs = c("FBI", "Voluntary", "Incident-level", as.character(round(14/38,3)), "Arrests Only", "All Incidences Reported",
                              "No", "Not Used"),
                    ucr = c("FBI", "Voluntary", "Monthly", as.character(round(30/38, 3)), "None","Hierarchy Rule",
                            "No", "Not Used"))

data_used_table <- data_used %>% 
  gt() %>% 
  tab_header(title = "Comparison of Relevant Data Sources") %>% 
  tab_style(
    style = cell_text(color = "darkgreen"),
    locations = cells_body(
      columns = c(daily_crime_log, clery),
      rows = daily_crime_log == "By-law"
    )) %>% 
  tab_style(
    style = cell_text(color = "red"),
    locations = cells_body(
      columns = c(nibrs, ucr),
      rows = nibrs == "Voluntary"
    )) %>% 
  tab_style(
    style = cell_text(color = "darkgreen"),
    locations = cells_body(
      columns = c(daily_crime_log, clery),
      rows = daily_crime_log == "1.000"
    )) %>% 
  tab_style(
    style = cell_text(color = "darkgreen"),
    locations = cells_body(
      columns = c(daily_crime_log, nibrs),
      rows = daily_crime_log == "Incident-level"
    )) %>% 
  tab_style(
    style = cell_text(color = "red"),
    locations = cells_body(
      columns = c(clery, ucr),
      rows = clery == "Yearly" | ucr == "Monthly"
    )) %>% 
  tab_style(
    style = cell_text(color = "red"),
    locations = cells_body(
      columns = c(nibrs, ucr),
      rows = nibrs == "0.368" | ucr == "0.789"
    )) %>% 
  tab_spanner(label = "Data Source", columns = c(2:5)) %>% 
  cols_label(attributes = " ", daily_crime_log = "Daily Crime Logs", clery = "Campus Safety Security", nibrs = "NIBRS", 
             ucr = "UCR") 
