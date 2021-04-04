############################################################################
##  This file takes the Rutgers pdf data and cleans it to desired format  ##
############################################################################

load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Rutgers/crime.rda")


rutgers <- all_crime %>% 
  select(-disposition) %>% 
  rename("case_number" = incident_number, 
         "incident" = nature, 'date_reported' = report_date,
         "date_occurred" = occurrence_date, "location" = general_location) %>%
  extract(date_reported, into = "time_reported", regex = "(\\d\\d\\d\\d)", remove = F) %>% 
  extract(date_occurred, into = "time_occurred", regex = "(\\d\\d\\d\\d)", remove = F) %>% 
  extract(date_reported, into = "date_reported", regex = "(\\d\\d/\\d\\d/\\d\\d)") %>% 
  extract(date_occurred, into = "date_occurred", regex = "(\\d\\d/\\d\\d/\\d\\d)") %>% 
  mutate(time_reported = format(strptime(time_occurred,"%H%M"), format = "%H:%M")) %>% 
  mutate(time_occurred = format(strptime(time_occurred, "%H%M"), format = "%H:%M")) %>% 
  mutate(date_reported = mdy(date_reported), date_occurred= mdy(date_occurred)) %>% 
  mutate(university = "Rutgers University-New Brunswick")

rutgers <- rutgers %>% 
  group_by(across(-incident)) %>% 
  summarize(incident = paste(incident, collapse = " ")) %>% 
  ungroup()


write_csv(rutgers, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/rutgers.csv")
  