library(tidyverse)
library(pdftools)
library(lubridate)

# directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Tufts/Medford Crime Log 2014"
# 
# directory_files <- list.files(directory)


tufts_crime <- tibble(date_occurred = character(), incident = character(), date_r = character())
years <- c(2014:2019)
for (year in years) {
  print(year)
  directory <- paste("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Tufts/Medford Crime Log ",year, sep = "")
  directory_files <- list.files(directory)
  for (i in seq_along(directory_files)) {
    path <- paste(directory, "/", directory_files[[i]], sep = "")
    print(path)
    tufts <- pdf_text(path)
    
    tufts %>% 
      str_split('\n') %>% 
      unlist() %>% str_to_lower() %>% str_trim() -> tufts
    
    tufts %>% 
      str_detect("^reported on") %>% which -> reported_indice
    ## goes from the date (e.g. the index of each date reported) to the end of the date reported indices
    for (date in 1:(length(reported_indice) -1)){
      ## this goes through the indices between each of the reported on indices and finds where the incidents are.
      for (i in (reported_indice[date] + 1): (reported_indice[date + 1] - 1)){
        if (str_detect(tufts[i], pattern = "^(incident|enforcement)")){
          crimes <- tibble("date_occurred" = tufts[reported_indice[date]],"incident" = tufts[i+1], "date_r" = tufts[i])
          tufts_crime <- tufts_crime %>% bind_rows(crimes)
        }
      }
    }
    
    ## note that the last loop does not take care of the final reported date. This loop is to account for that
    for (i in reported_indice[length(reported_indice)]: length(tufts)) {
      if (i == reported_indice[length(reported_indice)]) {
        date = i
      }
      if (str_detect(tufts[i], pattern = "^(incident|enforcement)")){
        crimes <- tibble("date_occurred" = tufts[date],"incident" = tufts[i +1], "date_r" = tufts[i])
        tufts_crime <- tufts_crime %>% bind_rows(crimes)
      }
    }
  }
}





### now I'm taking care of the other last files 

path <-
  "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Tufts/Tufts Problem Logs/crime_2015_81.pdf"
print(path)
tufts <- pdf_text(path)

tufts %>%
  str_split('\n') %>%
  unlist() %>% str_to_lower() %>% str_trim() -> tufts

tufts <- gsub("\003", " ", tufts)
tufts %>%
  str_detect("^reported on") %>% which -> reported_indice

tufts
## goes from the date (e.g. the index of each date reported) to the end of the date reported indices
for (date in 1:(length(reported_indice) - 1)) {
  ## this goes through the indices between each of the reported on indices and finds where the incidents are.
  for (i in (reported_indice[date] + 1):(reported_indice[date + 1] - 1)) {
    if (str_detect(tufts[i], pattern = "^(incident|judicial|enforcement)")) {
      crimes <-
        tibble(
          "date_occurred" = tufts[reported_indice[date]],
          "incident" = tufts[i + 1],
          "date_r" = tufts[i]
        )
      tufts_crime <- tufts_crime %>% bind_rows(crimes)
    }
  }
}

## note that the last loop does not take care of the final reported date. This loop is to account for that
for (i in reported_indice[length(reported_indice)]:length(tufts)) {
  if (i == reported_indice[length(reported_indice)]) {
    date = i
  }
  if (str_detect(tufts[i], pattern = "^(incident|judicial|enforcement)")) {
    crimes <-
      tibble(
        "date_occurred" = tufts[date],
        "incident" = tufts[i + 1],
        "date_r" = tufts[i]
      )
    tufts_crime <- tufts_crime %>% bind_rows(crimes)
  }
}




## need to add in the distinct() function to get rid of duplicates since the crime logs have duplicates everywhere
tufts_final <- tufts_crime %>% 
  extract(date_r, into = c("date_reported"), regex = "(\\d\\d/\\d\\d/\\d\\d)", remove = F ) %>% 
  extract(date_r, "case_number", "(\\d{2}tum.\\d{1,}.[ofar]{1,})", remove = F) %>% 
  extract(date_r, into = "time_occurred", regex = "(\\d\\d:\\d\\d)") %>% 
  mutate(date_occurred = str_trim(gsub("reported on", "", date_occurred))) %>% 
  mutate(date_occurred = str_trim(gsub("friday|monday|tuesday|thursday|saturday|sunday|wednesday", "", date_occurred))) %>% 
  mutate(date_occurred = str_trim(gsub("^,", "", date_occurred))) %>% 
  mutate(date_occurred = mdy(date_occurred)) %>% 
  mutate(time_reported = NA, location = NA, university = "Tufts University") %>% 
  mutate(date_reported = mdy(date_reported)) %>% 
  rename("date_r" = date_reported,   "date_o" = date_occurred) %>% 
  rename("date_occurred" = date_r, "date_reported" = "date_o") %>% 
  group_by(case_number) %>% 
  filter(!duplicated(case_number)) %>% 
  ungroup()

write_csv(tufts_final, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/tufts.csv")
