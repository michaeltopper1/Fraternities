
library(rvest)

years <- c(2013:2019)
months <- c("01", "02", "03",
            "04", "05", "06", 
            "07", "08", "09",
            "10", "11", "12")

for (year in years){
  print(year)
  for (month in months) {
    print(month)
    page <- read_html(paste("https://murraystate.edu/crimelog/index.cfm?year=", year, "&month=", month, sep = ""))
    if (month == "01" & year == 2013) {
      html_text(page) %>% str_split("\t") %>% unlist() -> month_text
      crime_indices <- month_text %>% 
        str_detect("Crime:") %>% which
      crime <- month_text[crime_indices] %>% 
        str_split_fixed("(Crime|General Location|Incident Description|Incident Number|Date of Incident|Time of Incident|Date Reported to Murray State Police|Time Reported to Murrary State Police):", n = 9) %>% 
        as_tibble()
    }
    else{
      html_text(page) %>% str_split("\t") %>% unlist() -> month_text
      crime_indices <- month_text %>% 
        str_detect("Crime:") %>% which
      crime_append <- month_text[crime_indices] %>% 
        str_split_fixed("(Crime|General Location|Incident Description|Incident Number|Date of Incident|Time of Incident|Date Reported to Murray State Police|Time Reported to Murrary State Police):", n = 9) %>% 
        as_tibble()
      crime <- crime %>% 
        bind_rows(crime_append)
    }
  }
}

final_crime <- crime %>% 
  separate(col = V8, into = c("date_reported", "time_reported"), sep = "Time Reported to Murray State Police:") %>% 
  select(-V1, -V4, -V9) %>% 
  rename("crime" = V2, "location" = V3, "incident_number" = V5, "date_of_incident" = V6, "time_of_incident" = V7) 

save(final_crime, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Murray State/crime.rda")
# crime %>% 
#   as_tibble()
# 
# html_text(page) %>% str_split("\t") %>% unlist() -> text
#  
# text %>%  str_detect("Crime:") %>% 
#   which -> crime_indices
# 
# text[crime_indices] %>% 
#   str_split_fixed("(Crime|General Location|Incident Description|Incident Number|Date of Incident|Time of Incident|Date Reported to Murray State Police|Time Reported to Murrary State Police):", n = 9) %>% 
#   View()
