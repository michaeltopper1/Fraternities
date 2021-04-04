#######################################################################################################
##  This file takes all the years and puts them from pdf to table. I have not cleaned the data yet.  ##
#######################################################################################################



library(tabulizer)
library(tidyverse)
library(pdftools)


years <- c(2013:2019)
for (year in years) {
  print(year)
  path <- paste("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Miami University/Crime and Fire Log ", year, ".pdf", sep = "")
  miami <- pdf_text(path)
  if (year == 2013) {
    total_crime <- map_dfr(miami, ~{
      str_split(., "\n") %>% unlist() -> miami_list;
      miami_list %>% str_detect("Incident Type:") %>% which -> indices_crimes
      miami_list %>% str_detect("Case #:") %>% which -> indices_cases
      miami_list %>% str_detect("Date/Time Reported:") %>% which -> indices_time
      miami_list[indices_crimes] %>% as_tibble() %>% 
        separate(col = value, into = c("incident", "crime"), sep = "\\s{3,}") %>% 
        mutate(crime = str_trim(crime)) %>% 
        select(crime) -> crimes
      miami_list[indices_cases] %>%
        as_tibble() %>%
        separate(col = value, into = c("case", "case_number", "disposition"), sep = "\\s{4,}") %>%
        select(case_number) -> cases
      miami_list[indices_time] %>%
        as_tibble() %>%
        separate(value, into = c('date_time', 'reported', 'date_occurred'), sep = ':') %>%
        separate(reported, into = c('date_reported', 'drop'), sep = "\\s{3,}") %>%
        select(date_reported, date_occurred) %>%
        mutate(across(everything(), ~str_trim(.))) -> time
      crimes <- crimes %>% 
        bind_cols(cases,time)
    })
  }
  else{
    total_crime_append <- map_dfr(miami, ~{
      str_split(., "\n") %>% unlist() -> miami_list;
      miami_list %>% str_detect("Incident Type:") %>% which -> indices_crimes
      miami_list %>% str_detect("Case #:") %>% which -> indices_cases
      miami_list %>% str_detect("Date/Time Reported:") %>% which -> indices_time
      miami_list[indices_crimes] %>% as_tibble() %>% 
        separate(col = value, into = c("incident", "crime"), sep = "\\s{3,}") %>% 
        mutate(crime = str_trim(crime)) %>% 
        select(crime) -> crimes
      miami_list[indices_cases] %>%
        as_tibble() %>%
        separate(col = value, into = c("case", "case_number", "disposition"), sep = "\\s{4,}") %>%
        select(case_number) -> cases
      miami_list[indices_time] %>%
        as_tibble() %>%
        separate(value, into = c('date_time', 'reported', 'date_occurred'), sep = ':') %>%
        separate(reported, into = c('date_reported', 'drop'), sep = "\\s{3,}") %>%
        select(date_reported, date_occurred) %>%
        mutate(across(everything(), ~str_trim(.))) -> time
      crimes <- crimes %>% 
        bind_cols(cases,time)
    })
    total_crime <- total_crime %>% 
      bind_rows(total_crime_append)
  }
}



# total_crime <- map_dfr(miami, ~{
#   str_split(., "\n") %>% unlist() -> miami_list;
#   miami_list %>% str_detect("Incident Type:") %>% which -> indices_crimes
#   miami_list %>% str_detect("Case #:") %>% which -> indices_cases
#   miami_list %>% str_detect("Date/Time Reported:") %>% which -> indices_time
#   miami_list[indices_crimes] %>% as_tibble() %>% 
#     separate(col = value, into = c("incident", "crime"), sep = "\\s{3,}") %>% 
#     mutate(crime = str_trim(crime)) %>% 
#     select(crime) -> crimes
#   miami_list[indices_cases] %>%
#     as_tibble() %>%
#     separate(col = value, into = c("case", "case_number", "disposition"), sep = "\\s{4,}") %>%
#     select(case_number) -> cases
#   miami_list[indices_time] %>%
#     as_tibble() %>%
#     separate(value, into = c('date_time', 'reported', 'date_occurred'), sep = ':') %>%
#     separate(reported, into = c('date_reported', 'drop'), sep = "\\s{3,}") %>%
#     select(date_reported, date_occurred) %>%
#     mutate(across(everything(), ~str_trim(.))) -> time
#   crimes <- crimes %>% 
#     bind_cols(cases,time)
#   })

save(total_crime, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Miami University/total_crime.rda")
