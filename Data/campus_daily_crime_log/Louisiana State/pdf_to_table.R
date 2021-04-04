library(pdftools)
library(tabulizer)
library(tidyverse)

path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Louisiana State/2013_Crime_log.pdf"
louis <- pdf_text(path)


new <- louis[2]
new <- gsub("\\s+LSU Police Crime Log", "", new)
new <- gsub("Page \\d+ of \\d+", "", new)
new <- gsub("Case Status", " ", new)
new <- gsub("Inactive", " ", new) 
new <- gsub("Pending", " ", new)
new <- gsub("Cleared", " ", new)
new <- gsub('\n\n', "", new)
new_1 <- str_split(new, pattern = "\n")



# first three columns
case_numbers <- tibble(
  "r1"=new_1[[1]][which(new_1[[1]] == new_1[[1]][8]) + 1] 
)

case_numbers <- case_numbers %>% 
  mutate(r1 = str_trim(r1)) %>% 
  separate(col = r1, into = c("case_number", "date_report"), sep = "\\s{2,}", remove = T) %>%
  extract(col = date_report, into = "year", regex = "(\\d\\d\\d\\d)", remove = F) %>% 
  separate(col = date_report, into = c("date_report","date_incident"), sep = ", 2013", remove = T) %>% 
  mutate(date_report = paste(date_report, ", ", year, sep = "")) %>% 
  select(-year)


new_1
# second three columns
new_1[[1]][which(new_1[[1]] == new_1[[1]][28]) + 1] 

offense <- tibble(
  "offense" = new_1[[1]][which(new_1[[1]] == new_1[[1]][28]) + 1] 
)

offense <- offense %>% 
  mutate(offense = str_trim(offense)) %>% 
  separate(col = offense, into = c("offense", "date_incident_end"), sep = "\\s{2,}", remove  = T)

case_numbers <- case_numbers %>% 
  bind_cols(offense)
