
library(tidyverse)
library(pdftools)
library(tabulizer)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/West Virigina/crime_reports.pdf"

## creating a list of pages to go through - the tabulizer package can only handle
## about 100 pages at a time
indice_list <- list(c(1:100), c(101:200), c(201:300), c(301:400),
                    c(401:500), c(501:600), c(601:700), c(701:800),
                    c(801:900), c(901:1000), c(1001:1100), c(1101:1200),
                    c(1201:1300), c(1301:1400), c(1401:1500), c(1501:1600),
                    c(1601:1700), c(1701:1800), c(1800:1848))

## looping through each of the lists and each of the pages within each of the lists
for (i in seq_along(indice_list)){
  print(i)
  west_virginia <- extract_tables(path, pages = indice_list[[i]])
  for (page in seq_along(indice_list[[i]])){
    if (page == 1) {
      crime <- west_virginia %>% 
        pluck(1) %>% 
        as_tibble()  
    }
    else {
      crime_append <- west_virginia %>% 
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
                  crime_6, crime_7, crime_8, crime_9, crime_10,
                  crime_11, crime_12, crime_13, crime_14, crime_15,
                  crime_16, crime_17, crime_18, crime_19)

## getting only the first 4 columns - the last few are wonky and I don't need them
crime_columns <- all_crime %>% 
  map(~select(.,V1, V2, V3, V4)) 

## saving the data set while getting rid of missing data in the description.
## missing data is caused by the nature of the document (e.g. weird spacings)
final_crime <- crime_columns %>% 
  reduce(bind_rows) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  janitor::clean_names() %>% 
  filter(description !="")

## saving
save(final_crime, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/West Virigina/crime.rda")

