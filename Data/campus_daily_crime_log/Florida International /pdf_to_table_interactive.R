
## interactive pdf table 
library(tabulizer)
library(tidyverse)

years <- c(2015:2019)
months <-
  c("01",
    "02",
    "03",
    "04",
    "05",
    "06",
    "07",
    "08",
    "09",
    "10",
    "11",
    "12")

## iterating through year and month
for (year in years) {
  print(year)
  for (month in months) {
    print(month)
    file_path <-
      paste("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida International /",year,"_",month,".pdf",sep = "")
    ## I skip a few years that only have 1 page, not two pages
    florida_international <- extract_areas(file_path, pages = 1, output = "csv", outdir =  "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Florida International /")
  }
}
    
    
    
    
    