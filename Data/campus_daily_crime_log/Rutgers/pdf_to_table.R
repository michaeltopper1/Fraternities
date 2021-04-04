library(pdftools)
library(tabulizer)
library(tidyverse)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Rutgers/CleryDailyCrimeLog (4).pdf"

indice_list <- list(c(1:100), c(101:200), c(201:300), c(301:400),
                    c(401:500), c(501:600), c(601:700), c(701:800),
                    c(801:900), c(901:953))

for (i in seq_along(indice_list)){
  print(i)
  rutgers <- extract_tables(path, method = "lattice", pages = indice_list[[i]])
  for (page in seq_along(indice_list[[i]])){
    if (page == 1) {
      crime <- rutgers %>% 
        pluck(1) %>% 
        as_tibble()  
    }
    else {
      crime_append <- rutgers %>% 
        pluck(page) %>% 
        as_tibble() 
      crime <- crime %>% 
        bind_rows(crime_append)
    }
  }
  block <- paste("crime_", i, sep = "")
  assign(block, crime)
  rm(crime)
  rm(crime_append)
}

## putting all the tibbles into a list so I can perform a map functiont o simultaneously select
all_crime <- list(crime_1, crime_2, crime_3, crime_4, crime_5,
                  crime_6, crime_7, crime_8, crime_9, crime_10)

all_crime <- all_crime %>% 
  reduce(bind_rows) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  janitor::clean_names()

## since row to names only works on the first column, i had to get rid of all the columns that got put in. there were 952 of these which matched the number of pages.
all_crime <- all_crime %>% 
  filter(incident_number != "Incident\rNumber") 

save(all_crime, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Rutgers/crime.rda")
