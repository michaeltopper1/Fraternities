library(tidyverse)
library(tabulizer)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Texas State/UPD Legacy Crime and Fire Log Data 2013-2019.pdf"
texas_state <- extract_tables(path)

for (i in 1:length(texas_state)) {
  if (i == 1) {
    crime_table <- texas_state %>% 
      pluck(i) %>% 
      as_tibble() %>% 
      janitor::row_to_names(row_number = 1) 
  }
  else {
    crime_to_append <- texas_state %>% 
      pluck(i) %>% 
      as_tibble 
    colnames(crime_to_append) <- colnames(crime_table)
    crime_table <- crime_table %>% 
      bind_rows(crime_to_append)
  }
}

path_2019_only <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Texas State/UPD Crime and Fire Log Data 2019.pdf"
texas_state_2019_only <- extract_tables(path_2019_only)

for (i in 1:length(texas_state_2019_only)) {
  if (i == 1) {
    crime_table_2019 <- texas_state_2019_only %>% 
      pluck(i) %>% 
      as_tibble()
  }
  else {
    crime_to_append_2019 <- texas_state_2019_only %>% 
      pluck(i) %>% 
      as_tibble() 
    colnames(crime_to_append_2019) <- colnames(crime_table_2019)
    crime_table_2019 <- crime_table_2019 %>% 
      bind_rows(crime_to_append_2019)
  }
}

## saving the data from 2013- 2019 august. Going to have to append.
save(crime_table, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Texas State/texas_1.rda")

## saving the 2019 only data - waitingn for follow up on data. Going to have to merge these two together just now sure how
save(crime_table_2019, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Texas State/texas_2.rda")

