library(rvest)
library(tidyverse)
library(lubridate)

### note I broke this up but not in the code. You'll need to change the dates of the loop to loop through what you want

test_forcolnames <- jsonlite::read_json("https://www.dpss.umich.edu/api/GetCrimeLogCache?date=02/24/2021")

grab_colnames <- map(test_forcolnames$data, ~as_tibble(.)) %>% 
  reduce(bind_rows) 

##creating an empty tibble of the columnnames that will show up after pulling from the API
michigan <- tibble(colnames(grab_colnames)[1],colnames(grab_colnames)[2],
                   colnames(grab_colnames)[3], colnames(grab_colnames)[4],
                   colnames(grab_colnames)[5], colnames(grab_colnames)[6],
                   colnames(grab_colnames)[7], colnames(grab_colnames)[8],
                   colnames(grab_colnames)[9], colnames(grab_colnames)[10]) %>% 
  janitor::row_to_names(row_number = 1)

## creating a vector of dates to loop through - did this in chucks. 
dates <- seq(from = ymd('2018-01-01'), to= ymd('2019-12-31'), by = 'days')
dates <- format(as.Date(dates),'%m/%d/%Y')

## creating an empty list just to check when there are no crimes to report
empty_list <- list()

## pulling from the API
for (date in dates) {
  print(date)
  Sys.sleep(5)
  link <- paste("https://www.dpss.umich.edu/api/GetCrimeLogCache?date=", date, sep = "")
  crime_log <- jsonlite::read_json(link)
  if (identical(crime_log$data, empty_list)){
    next
  }
  else {
    crime_to_append <- map(crime_log$data, ~as_tibble(.)) %>% 
      reduce(bind_rows)
    michigan <- michigan %>% 
      bind_rows(crime_to_append)
  }
}

# write_csv(michigan, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Michigan/michigan_13_17.csv")
write_csv(michigan, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Michigan/michigan_18_19.csv")
