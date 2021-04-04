## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-03-10
##

library(tidyverse)
library(textreadr)
library(pdftools)
library(lubridate)
pattern_loc <- "^i\\s{0,1}n\\s{0,1}c\\s{0,1}i\\s{0,1}d\\s{0,1}e\\s{0,1}n\\s{0,1}t\\s{0,1}a\\s{0,1}d\\s{0,1}d\\s{0,1}r\\s{0,1}e\\s{0,1}s\\s{0,1}s"
pattern_tr <- "^t\\s{0,1}i\\s{0,1}m\\s{0,1}e\\s{0,1}r\\s{0,1}e\\s{0,1}p\\s{0,1}o\\s{0,1}r\\s{0,1}t\\s{0,1}e\\s{0,1}d"
pattern_to <- "^t\\s{0,1}i\\s{0,1}m\\s{0,1}e\\s{0,1}o\\s{0,1}c\\s{0,1}c\\s{0,1}u\\s{0,1}r\\s{0,1}r\\s{0,1}e\\s{0,1}d"
## extracts from the docx files into a cleaned tibble
doc_to_table <- function(path) {
  wsu <- read_document(path)
  print(path)
  wsu %>% str_to_lower %>% str_replace_all(pattern = "\\s+", " ") %>% str_trim() -> wsu
  wsu %>% str_detect("^\\d\\s{0,1}\\d\\s{0,1}-\\s{0,1}w") %>% which -> case_number
  wsu %>% str_detect(pattern_loc) %>% which ->location
  wsu %>% str_detect(pattern_tr) %>% which -> time_reported
  wsu %>% str_detect(pattern_to) %>% which-> time_occurred
  location <- wsu[location] %>% 
    as_tibble() %>% 
    separate(value, c("words", "location"), sep = ":") %>% 
    select(-words) %>% 
    mutate(across(everything(), ~str_trim(.)))
  case_number <- wsu[case_number] %>% 
    as_tibble() %>% 
    extract(value, c("case_number", "incident"), "(\\d\\d-w\\s{0,1}\\d{1,4}\\s{0,1}\\d{1,4})\\s(.+)") %>% 
    mutate(across(everything(), ~str_trim(.)))
  time_reported <- wsu[time_reported] %>% 
    as_tibble() %>% 
    extract(value, c("time_reported", "date_reported"), "(\\d{1,2}\\s{0,1}:\\s{0,1}\\d{1,2})\\s(\\d{1}\\s{0,1}\\d{1}\\s{0,1}/\\d{1}\\s{0,1}\\d{1}\\s{0,1}/\\s{0,1}\\d{1}\\s{0,1}\\d{1})") %>% 
    mutate(across(everything(), ~gsub(" ", "", .)))
  time_occurred <- wsu[time_occurred] %>% 
    as_tibble() %>% 
    extract(value, "time_occurred", "(\\d{1,2}:\\d{1,2})", remove = F) %>% 
    extract(value, "date_occurred", "(\\d{1,2}/\\d{1,2}/\\d{1,2})") %>% 
    mutate(across(everything(), ~gsub(" ", "",. )))
  wsu_final <- bind_cols(location, case_number, time_reported, time_occurred)
  return(wsu_final)
}
## gets in the pdfs to a cleaned tibble
pdf_to_table <- function(path){
  print(path)
  ws <- pdf_text(path)
  ws %>% str_split('\n') %>% unlist() %>% str_trim() -> ws
  ws %>% str_detect("^\\d\\d-W") %>% which -> case_number
  ws %>% str_detect("^Occur") %>% which  -> occurred
  ws[case_number] %>% 
    as_tibble() %>% 
    extract(value, c("case_number", "time_reported", "date_reported", "incident"),
            '(\\d{2}-W\\s{0,1}\\d\\s{0,1}\\d\\s{0,1}\\d\\s{0,1}\\d)\\s{1,}(\\d{1,2}:\\d{1,2}:\\d{1,2})\\s(\\d{1,2}/\\d{1,2}/\\d{2})\\s(.{1,})') %>% 
    separate(incident, c('incident', 'location'), "\\s{3,}", extra = "merge") -> case_number
  return(case_number)
}

months <- month.name
years <- c(2014:2015)
for (year in years) {
  print(year)
  upper_directory <- paste("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Washington State/WSUPD ",year," Press Log", sep = "")
  for (month in months) {
    print(month)
    directory <- paste(upper_directory, "/", month, " ", year, sep = "")
    print(directory)
    setwd(directory)
    wsu <- map(list.files(directory, pattern = "\\d\\d\\d\\d.docx"), ~doc_to_table(.)) %>% 
      reduce(bind_rows)
    name <- paste(month, "_", year, sep = "")
    assign(name, wsu)
  }
}
## binds together all of 2014 and 2015
wsu_14_15 <- bind_rows(January_2014, January_2015,
                       February_2014, February_2015,
                       March_2014, March_2015,
                       April_2014, April_2015,
                       May_2014, May_2015,
                       June_2014, June_2015,
                       July_2014, July_2015,
                       August_2014, August_2015,
                       September_2014, September_2015,
                       October_2014, October_2015,
                       November_2014, November_2015,
                       December_2014, December_2015)


### gets in the remaining docx files that spill over into 2016 - jan, feb, and half of march
jan_2016 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Washington State/WSUPD 2016 Press Log/January 2016"
feb_2016 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Washington State/WSUPD 2016 Press Log/February 2016"
march_2016_1 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Washington State/WSUPD 2016 Press Log/March 2016"

setwd(jan_2016)
jan_2016 <- map(list.files(jan_2016, pattern = "\\d\\d\\d\\d.docx$"), ~doc_to_table(.)) %>% 
  reduce(bind_rows)
setwd(feb_2016)
feb_2016 <- map(list.files(feb_2016, pattern = "\\d\\d\\d\\d.docx"), ~doc_to_table(.)) %>% 
  reduce(bind_rows)
setwd(march_2016_1)
march_2016_1 <- map(list.files(march_2016_1, pattern = "\\d\\d\\d\\d.docx"), ~doc_to_table(.)) %>% 
  reduce(bind_rows)
## binds together the 3 months of 2016 that have .docx extensions
wsu_2016_janfebmarch <- bind_rows(jan_2016, feb_2016, march_2016_1)

#### now inputting march 2016 - december 2019.
years <- c(2016:2019)
for (year in years) {
  print(year)
  upper_directory <- paste("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Washington State/WSUPD ",year," Press Log", sep = "")
  for (month in months) {
    if ((month == "January" | month == "February") & year == 2016) {
      next
    }
    directory <- paste(upper_directory, "/", month, " ", year, sep = "")
    setwd(directory)
    wsu_pdf <- map(list.files(directory, pattern = "\\d\\d\\d\\d.pdf$"), ~pdf_to_table(.)) %>% 
      reduce(bind_rows)
    name <- paste(month, "_", year, sep = "")
    assign(name, wsu_pdf)
  }
}

wsu_16_19 <- bind_rows(January_2017, January_2018, January_2019,
                       February_2017, February_2018, February_2019,
                       March_2016, March_2017, March_2018, March_2019,
                       April_2016, April_2017, April_2018, April_2019,
                       May_2016, May_2017, May_2018, May_2019,
                       June_2016, June_2017, June_2018, June_2019,
                       July_2016, July_2017, July_2018, July_2019,
                       August_2016, August_2017, August_2018, August_2019,
                       September_2016, September_2017, September_2018, September_2019,
                       October_2016, October_2017, October_2018, October_2019,
                       November_2016, November_2017, November_2018, November_2019,
                       December_2016, December_2017, December_2018, December_2019)
wsu_16_19 <- wsu_16_19 %>% 
  mutate(date_occurred = NA, time_occurred= NA)

## inputting in the very last pdfs/.docx files that needed to be dealt with separately
feb_2018_16 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Washington State/WSUPD 2018 Press Log/February 2018/0216 - Additional Case.pdf"
feb_2018_16 <- pdf_to_table(feb_2018_16) %>% 
  mutate(date_occurred = NA, time_occurred = NA)
sept_2018_3 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Washington State/WSUPD 2018 Press Log/September 2018/0903 part 2.pdf"
sept_2018_3 <- pdf_to_table(sept_2018_3) %>% 
  mutate(date_occurred = NA, time_occurred = NA)

aug_2015_16 <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Washington State/WSUPD 2015 Press Log/August 2015/0816PROBLEM.docx"
aug_2015_16 <- read_document(aug_2015_16)

aug_2015_16 %>% str_to_lower %>% str_replace_all(pattern = "\\s+", " ") %>% str_trim() -> aug_2015_16
aug_2015_16 %>% str_detect("^\\d\\s{0,1}\\d\\s{0,1}-\\s{0,1}w") %>% which -> case_number_aug
aug_2015_16 %>% str_detect(pattern_tr) %>% which -> time_reported_aug
aug_2015_16 %>% str_detect(pattern_to) %>% which-> time_occurred_aug

aug_2015_16[case_number_aug] %>% 
  as_tibble() %>% 
  extract(value, c('case_number', 'incident', 'location'), "(15-w\\d{4})\\s(.{1,})\\sincident address\\s:\\s(.{1,})") -> case_number_aug
aug_2015_16[time_reported_aug] %>% 
  as_tibble() %>% 
  extract(value, c("time_reported", "date_reported"),
          "(\\d\\d:\\d\\d) (\\d{1,2}/\\d{1,2}/\\d{1,2})") -> time_reported_aug
aug_2015_16 <- bind_rows(case_number_aug, time_reported_aug) %>% 
  mutate(date_occurred = NA, time_occurred = NA)


## Now binding everything together:
wsu_final <- bind_rows(wsu_14_15, wsu_16_19, feb_2018_16, sept_2018_3, aug_2015_16, wsu_2016_janfebmarch)

wsu_final <- wsu_final %>% 
  mutate(across(starts_with("date"), ~mdy(.))) %>% 
  mutate(university = "Washington State University") %>% 
  filter(!is.na(incident))

write_csv(wsu_final, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/wsu.csv")

