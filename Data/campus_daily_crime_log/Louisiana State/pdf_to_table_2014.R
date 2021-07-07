#############################################################################
##  This gets in all except the last page of the 2014 data from Louisiana  ##
#############################################################################


library(pdftools)
library(tabulizer)
library(tidyverse)

path <- "Data/campus_daily_crime_log/Louisiana State/2014crimelog.pdf"

louisiana <- pdf_text(path)
## withholding the last page since it ends early and the iteration gets off
iterations <- length(louisiana) 

for (i in 1:iterations){
  print(i)
  if (i == 1) {
    ## gsubing out certain things so I can get the correct format
    page_x <- louisiana[i]
    page_x <- gsub("\\s+LSU Police Crime Log", "", page_x)
    page_x <- gsub("Page \\d+ of \\d+", "", page_x)
    page_x <- gsub("Case Status", " ", page_x)
    page_x <- gsub("Inactive", " ", page_x) 
    page_x <- gsub("Pending", " ", page_x)
    page_x <- gsub("Cleared", " ", page_x)
    page_x <- gsub('\n\n', "", page_x)
    ## splitting by \n to get a nice pattern
    page_x <- str_split(page_x, pattern = "\n")
    ## creating a tibble such that the elements are the stuff below the columns I want
    first_columns <- tibble(
      "r1" = page_x[[i]][which(page_x[[i]] == page_x[[i]][2]) + 1]
    )
    ## I do a few things here: trim whitespace to make sure separate doesn't give me a warning
    ## then I separate by 2 or more whitespaces
    ## then I extract the year to make it it's own column: this is done because separate removes the separater
    ## then I separate on , year
    ## I mutate to put things into a nicer format
    first_columns <-  first_columns %>% 
      mutate(r1 = str_trim(r1)) %>% 
      separate(col = r1, into = c("case_number", "date_report"), sep = "\\s{2,}", remove = T) %>% 
      extract(col = date_report, into = "year", regex = "(\\d\\d\\d\\d)", remove = F) %>% 
      separate(col = date_report, into = c("date_report","date_incident"), sep = ", 2014", remove = T) %>% 
      mutate(date_report = paste(date_report, ", ", year, sep = "")) %>% 
      select(-year)
    second_columns <- tibble(
      "offense" = page_x[[i]][which(page_x[[i]] == page_x[[i]][4]) + 1]
    )
    second_columns <- second_columns %>% 
      mutate(offense = str_trim(offense)) %>% 
      separate(col = offense, into = c("offense", "date_incident_end"), sep = "\\s{2,}", remove  = T)
    crimes <- bind_cols(first_columns, second_columns)
  }
  else {
    page_x <- louisiana[i]
    page_x <- gsub("\\s+LSU Police Crime Log", "", page_x)
    page_x <- gsub("Page \\d+ of \\d+", "", page_x)
    page_x <- gsub("Case Status", " ", page_x)
    page_x <- gsub("Inactive", " ", page_x) 
    page_x <- gsub("Pending", " ", page_x)
    page_x <- gsub("Cleared", " ", page_x)
    page_x <- gsub('\n\n', "", page_x)
    page_x <- str_split(page_x, pattern = "\n")
    first_columns <- tibble(
      "r1" = page_x[[1]][which(page_x[[1]] == page_x[[1]][2]) + 1]
    )
    first_columns <-  first_columns %>% 
      mutate(r1 = str_trim(r1)) %>% 
      separate(col = r1, into = c("case_number", "date_report"), sep = "\\s{2,}", remove = T) %>% 
      extract(col = date_report, into = "year", regex = "(\\d\\d\\d\\d)", remove = F) %>% 
      separate(col = date_report, into = c("date_report","date_incident"), sep = ", 2014", remove = T) %>% 
      mutate(date_report = paste(date_report, ", ", year, sep = "")) %>% 
      select(-year)
    second_columns <- tibble(
      "offense" = page_x[[1]][which(page_x[[1]] == page_x[[1]][4]) + 1]
    )
    second_columns <- second_columns %>% 
      mutate(offense = str_trim(offense)) %>% 
      separate(col = offense, into = c("offense", "date_incident_end"), sep = "\\s{2,}", remove  = T)
    crimes_over_page_1 <- bind_cols(first_columns, second_columns)
    crimes <- crimes %>% 
      bind_rows(crimes_over_page_1)
  }
}

##saving as a different name just because
crime_2014 <- crimes
save(crime_2014, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Louisiana State/2014.rda")

