
library(tidyverse)
library(readxl)
library(lubridate)


read_excel_allsheets <- function(filename, tibble = TRUE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_xlsx(filename, sheet = X, skip = 2))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Marshall University/publicrecordsrequest_"
crime_reports <- list.files(directory)

## this first loop goes through all except 2016 and 2019
count = 0
for (i in crime_reports) {
  count = count + 1
  print(i)
  path <- paste(directory, "/", i, sep = "")
  if (path == "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Marshall University/publicrecordsrequest_/MUPD Crime Log 2019.xlsx"){
    next
  }
  if (path == "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Marshall University/publicrecordsrequest_/MUPD Crime Log 2016.xlsx"){
    next
  }
  if (count ==1) {
    marshall_crime <- read_excel_allsheets(path)[1:12]
    marshall_crime <- map(marshall_crime, ~janitor::clean_names(.) %>%
                            rename(.,"incident" = x1, "case_number" = number, "date_reported" = reported,
                                   "date_occurred" = occurred_4, "time_occurred" = occurred_5, "disposition" = x8, "location" = x6) %>%
                            select(.,-campus, -disposition) %>%
                            extract(.,time_occurred, into = "time_occurred", regex = "(\\d\\d:\\d\\d)") %>%
                            mutate(., date_reported = ymd(date_reported), date_occurred = ymd(date_occurred)) %>%
                            mutate(., university = "Marshall University"), time_reported = NA)
    marshall_crime <- marshall_crime %>%
      bind_rows()
  }
  else {
    marshall <- read_excel_allsheets(path)[1:12]
    marshall<- map(marshall, ~janitor::clean_names(.) %>%
                            rename(.,"incident" = x1, "case_number" = number, "date_reported" = reported,
                                   "date_occurred" = occurred_4, "time_occurred" = occurred_5, "disposition" = x8, "location" = x6) %>%
                            select(.,-campus, -disposition) %>%
                            extract(.,time_occurred, into = "time_occurred", regex = "(\\d\\d:\\d\\d)") %>%
                            mutate(., date_reported = ymd(date_reported), date_occurred = ymd(date_occurred)) %>%
                            mutate(., university = "Marshall University"), time_reported = NA)
    marshall <- marshall %>%
      bind_rows()
    marshall_crime <- marshall_crime %>%
      bind_rows(marshall)
  }
}



### now going through 2016 and 2019
path_2016 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Marshall University/publicrecordsrequest_/MUPD Crime Log 2016.xlsx"
marshall_2016 <- read_excel_allsheets(path_2016)[1:12]

marshall_2016 <- map(marshall_2016, ~ janitor::clean_names(.) %>% rename("incident" = x1,
                                                        "case_number" = number, "date_reported" = reported,
                                                        "date_occurred" = occurred_4,
                                                        "time_occurred" = occurred_5, "location" = x6) %>%
      extract(.,time_occurred, into = "time_occurred", regex = "(\\d\\d:\\d\\d)") %>%
      mutate(., date_reported = ymd(date_reported), date_occurred = ymd(date_occurred)) %>%
      mutate(., university = "Marshall University") %>% select(-campus, -x8))

marshall_2016 <- marshall_2016 %>% 
  bind_rows()



path_2019 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Marshall University/publicrecordsrequest_/MUPD Crime Log 2019.xlsx"

marshall_2019 <- read_excel_allsheets(path_2019)[1:12]

marshall_2019 <- map(marshall_2019, ~ janitor::clean_names(.) %>% rename("incident" = x1,
                                                                         "case_number" = number, "date_reported" = reported,
                                                                         "date_occurred" = occurred_4,
                                                                         "time_occurred" = occurred_5, "location" = x6) %>%
                       extract(.,time_occurred, into = "time_occurred", regex = "(\\d\\d:\\d\\d)") %>%
                       mutate(., date_reported = ymd(date_reported), date_occurred = ymd(date_occurred)) %>%
                       mutate(., university = "Marshall University") %>% select(-campus, -x8))

marshall_2019 <- marshall_2019 %>% 
  bind_rows()

marshall_total <- bind_rows(marshall_crime, marshall_2016, marshall_2019)
marshall_total <- marshall_total %>% 
  filter(!is.na(incident)) %>% 
  mutate(time_reported = NA)

write_csv(marshall_total, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/marshall.csv")
