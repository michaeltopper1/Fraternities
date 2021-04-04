#################################################################
##    This file brings in all data from 2014 to march 2015.    ##
#################################################################


library(pdftools)
library(tidyverse)

path_14 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Ball State/Log_2014.pdf"
path_15 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Ball State/Log_2015.pdf"

paths <- list(path_14, path_15)

pdf_to_table <- function(path) {
  ball <- pdf_text(path)
  ball %>% str_split("\n") %>% unlist() %>% str_trim() ->ball
  ball %>% str_detect("^\\d{1,2}/\\d{1,2}/\\d{0,4}") -> dates
  ball <- ball[dates] %>% 
    as_tibble() %>% 
    extract(value, c("date_reported", "time_reported", "incident", "case_number"),
            "(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s{1,}(\\d{1,2}:\\d{1,2}:\\d{1,2})(.{1,})(\\d{4}-\\d{1,})") %>% 
    mutate(incident = gsub("Suspended|Cleared", "", incident)) %>% 
    mutate(university = "Ball State University", date_occurred = NA, location = NA, time_occurred = NA)
  return(ball)
}


ball <- map(paths, ~pdf_to_table(.)) %>% 
  reduce(bind_rows)


write_csv(ball, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Ball State/ball_2013_2015.csv")
