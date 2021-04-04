
##################################################################################################################################
##  Crime from July 2015-2019. Need to be careful cleaning this data - lots of weird lines that have empty things form the PDF  ##
##################################################################################################################################


library(pdftools)
library(tabulizer)
library(tidyverse)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/University of North Florida/UPD Crime Log July 2015 - 2019.pdf"

north_florida <- extract_tables(path)

for (i in seq_along(north_florida)) {
  if (i ==1) {
    crime <- north_florida %>% 
      pluck(1) %>% 
      as_tibble() %>% 
      janitor::row_to_names(row_number = 1) %>% 
      janitor::clean_names()
  }
  else {
    crime_append <- north_florida %>% 
      pluck(i) %>% 
      as_tibble() %>% 
      filter(V1 != "Nature (Classification)")
    colnames(crime_append) <- colnames(crime)
    crime <- crime %>% 
      bind_rows(crime_append)
  }
}

north_florida_crime <- crime

write_csv(north_florida_crime, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/University of North Florida/crime_2015-2019.csv")
