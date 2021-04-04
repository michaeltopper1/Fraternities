library(tabulizer)
library(tidyverse)


## the pdf file path
crime_pdf <-  "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida Atlantic/21-010 PRR_2013-2019_FAU - PD Daily Police Blotter copy.pdf"

## had to specify the method to extract table:  note that I got rid of 1 row in this process - but it's not important to me
florida_crime <- extract_tables(crime_pdf, method = "lattice")

## extracting all the information and parsing it into a tibble
for (i in 1:length(florida_crime)) {
  if (i == 1) {
    crime_table <-  florida_crime %>%  pluck(i) %>% as_tibble()
  }
  else {
    append_table <-  florida_crime %>%  
      pluck(i) %>% 
      as_tibble() 
    crime_table <- crime_table %>% bind_rows(append_table)
  }
}

## renaming the column names
crime_table <-  crime_table %>% 
  rename(date_occurred = V1, time_occurred = V2, 
         date_reported = V3, case_number = V4, incident = V5,
         location = V6, campus = V7, status = V8)
export_path = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida Atlantic/final_crime.rda"
save(crime_table, file = export_path)
