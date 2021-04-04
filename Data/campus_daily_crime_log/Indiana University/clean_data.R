

library(tidyverse)
library(lubridate)

## changes working directory
directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Indiana University/"
setwd(directory)

## grabs all the .rda files in the director and loads them in
all_files <- list.files(directory, pattern = ".rda$")
for (i in seq_along(all_files)) {
  load(all_files[i])
}

## binds together all the tibbles
indiana <- bind_rows(crime_total_2014, crime_total_2015,
                     crime_total_2016, crime_total_2017,
                     crime_total_2018, crime_total_2019,
                     indiana_p_2014, indiana_p_2015, 
                     indiana_p_2016, indiana_p_2017,
                     indiana_p_2018, indiana_p_2019)

##changes the date format
indiana <- indiana %>% 
  mutate(across(starts_with("date"), ~ mdy(.)))

indiana <- indiana %>% 
  group_by(across(-incident)) %>% 
  summarize(incident = paste(incident, collapse = " ")) %>% 
  ungroup()

## saves
write_csv(indiana, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Cleaned_schools/indiana.csv")
