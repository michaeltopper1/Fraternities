
library(pdftools)
library(tidyverse)

path_2015_18 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Ball State/Log 3.19.15 to 12.31.18.pdf"
path_2019 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Ball State/Log 1.1.19 to 12.31.19.pdf"

paths <- list(path_2015_18,path_2019)
## cleans the pdfs
clean_pdf <- function(path){
  ball <- pdf_text(path)
  ball %>% str_split('\n') %>% unlist() %>% str_trim()  -> ball
  ball <- ball[-1]
  ball <- ball %>% 
    as_tibble() %>%
    extract(value, c("incident", "case_number", 'date_reported', "time_reported"),
            "(.{1,})(\\d{4}-{1,}\\d{6,})\\s{1,}(\\d{1,2}-\\d{1,2}-\\d{2,4}).{1,}(\\d{1,2}:\\d{1,2})") %>% 
    mutate(university = "Ball State University", "time_occurred" = NA, date_occurred = NA, location = NA)
  return(ball)
}

ball_state_15_19 <- map(paths, ~clean_pdf(.)) %>% 
  reduce(bind_rows)


write_csv(ball_state_15_19, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Ball State/ball_2015_2019.csv")

