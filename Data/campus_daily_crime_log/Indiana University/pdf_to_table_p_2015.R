
path <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Indiana University/5-2015/5-15-15.pdf"

indiana_p_2015 <- pdf_text(path)
indiana_p_2015 <- str_to_lower(indiana_p_2015)
indiana_p_2015 %>% str_split('\n') %>% unlist() -> indiana_p_2015
indiana_p_2015 <- str_trim(indiana_p_2015)
indiana_p_2015 %>% str_detect("student right to know cad daily log") %>% which -> right_to_know
indiana_p_2015
indiana_p_2015 %>% str_detect("^incident") %>% which -> incidents_indices
indiana_p_2015 %>% str_detect("^location|general location") %>% which -> location_indices
indiana_p_2015 %>% str_detect("^date reported") %>% which -> date_reported_indices
indiana_p_2015[incidents_indices] %>% 
  as_tibble() %>% 
  separate(value, into = c("value", "incident"), sep = ":\\s", extra = "merge") %>% 
  mutate(incident = str_trim(incident)) -> incidents

indiana_p_2015[location_indices] %>% 
  as_tibble() %>% 
  separate(value, c("value", "location"), sep = ":") %>% 
  mutate(location = str_trim(location)) %>% 
  select(location) -> locations

indiana_p_2015[date_reported_indices] %>% 
  as_tibble() %>% 
  extract(value, "time_reported", "(\\d\\d:\\d\\d)", remove = F) %>% 
  extract(value, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d\\d)", remove = F) %>% 
  extract(value, "case_number", "(\\d{3,10}$)") %>% 
  mutate(date_occurred = NA, time_occurred = NA) -> date_reported

indiana_p_2015 <- bind_cols(date_reported, incidents, locations)
indiana_p_2015 <- indiana_p_2015 %>% 
  select(-value) %>% 
  mutate(university = "Indiana University-Bloomington")

save(indiana_p_2015, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Indiana University/crime_p_2015.rda")
