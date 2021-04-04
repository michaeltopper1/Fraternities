library(tidyverse)
library(pdftools)
library(lubridate)
clemson <- pdf_text("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Clemson/2014-2018 Daily Crime Logs.pdf")

## unlists everything and trims the string and splits by new line
clemson %>% str_split('\n') %>% unlist() %>% str_trim() ->clemson
## gets rid of any case status, or column headers that can be found on the page - necessary for the algorithm
clemson %>% str_detect("^Case|^Page") %>% which -> unneeded_info
## gets rid of the unneeded info
clemson <- clemson[-unneeded_info]
## gets rid of the empty lines
clemson <- clemson[clemson != ""]
## extracts the case numbers
clemson %>% 
  str_detect("^.-\\d{2}-\\d{1,}") %>% which -> case_numbers


clemson[-unneeded_info]
clemson %>% str_remove_all("^Case|^Page")

clemson_tibble = tibble(case_number = as.character(), 
                        incident = as.character(),
                        date_reported = as.character(),
                        time_reported = as.character())

for (case_indice in case_numbers) {
  for (row in (case_indice + 1):length(clemson)) {
    if (clemson[row] %>% str_detect("^\\d{2,}")) {
      clemson_tibble <- clemson_tibble %>% 
        add_row(case_number = str_extract(clemson[case_indice], "P-\\d{1,2}-\\d{1,}"),
                incident = clemson[row],
                date_reported = str_extract(clemson[case_indice], "\\d{1,2}/\\d{1,2}/\\d{1,2}"),
                time_reported = str_extract(clemson[case_indice], "\\d{1,2}:\\d{1,2}:\\d{1,2}[AP]M.{1,}"))
    }
    else {
      break
    }
  }
}

clemson_tibble <- clemson_tibble %>% 
  extract(time_reported, c("time_reported", "date_occurred", "time_occurred", "location"),
          "(\\d{1,2}:\\d{1,2}:\\d{1,2}[AP]M)\\s{1,}(\\d{1,2}/\\d{1,2}/\\d{2,4})\\s{1,}(\\d{1,2}:\\d{1,2}:\\d{1,2}[AP]M)\\s{1,}(.{1,})")

clemson_14_18 <- clemson_tibble %>% 
  group_by(case_number, date_reported, date_occurred, time_occurred, location, time_reported) %>% 
  summarize(incident = paste(incident, collapse = " "))  %>% 
  ungroup() %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(across(starts_with("time"), ~format(strptime(., "%I:%M:%S%p"), format = "%H:%M"))) %>% 
  mutate(university = "Clemson University")

write_csv(clemson_14_18, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Clemson/clemson_14_18.csv")
