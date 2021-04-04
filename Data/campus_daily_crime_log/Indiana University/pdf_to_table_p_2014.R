##################################################################
##        This files puts in the problem files from 2014        ##
##################################################################

##note that this file was a little different so it was not in the problem files 


path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Indiana University/12-6-14.pdf"

indiana_p_2014 <- pdf_text(path)

indiana_p_2014 <- str_to_lower(indiana_p_2014)
indiana_p_2014 %>% str_split('\n') %>% unlist() -> indiana_p_2014
indiana_p_2014 <- str_trim(indiana_p_2014)
indiana_p_2014 %>% head(20)
indiana_p_2014 %>% str_detect("^incident") %>% which -> incidents_indices
indiana_p_2014 %>% str_detect("^date and time") %>% which -> dates_and_time_indices
indiana_p_2014 %>% str_detect("^date reported") %>% which -> date_reported_indices

indiana_p_2014[incidents_indices] %>% 
  as_tibble() %>% 
  separate(value, into = c("value", "incident"), sep = "\\s:\\s", extra = "merge") %>% 
  separate(incident, into = c("incident", "case_number"), sep = "\\s{4,}") %>% 
  extract(case_number, "case_number", "(\\d{1,10}$)") -> incidents
incidents <- incidents[-9,]
indiana_p_2014[dates_and_time_indices] %>% 
  as_tibble() %>% 
  extract(value, "date_occurred", "(\\d{1,}/\\d{1,2}/\\d{2})", remove = F) %>% 
  extract(value, "time_occurred", "(\\d\\d:\\d\\d)") -> dates_and_time

indiana_p_2014[date_reported_indices] %>% 
  as_tibble() %>% 
  separate(value, into = c("date_reported", "location"), sep = "location : ") %>% 
  extract(location, "location", "(.{1,}\\s{3,})") %>% 
  extract(date_reported, "time_reported", "(\\d\\d:\\d\\d)", remove = F) %>% 
  extract(date_reported, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d\\d)") %>% 
  mutate(location = str_trim(location)) -> date_reported

indiana_p_2014 <- bind_cols(incidents, date_reported, dates_and_time)

indiana_p_2014 <- indiana_p_2014 %>% 
  select(-value) %>% 
  mutate(university = "Indiana University-Bloomington")
save(indiana_p_2014, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Indiana University/crime_p_2014.rda")
