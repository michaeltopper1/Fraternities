########################################################################################
##   ##
########################################################################################



directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Indiana University/"

vector.is.empty <- function(x) return(length(x) ==0 )
months <- c(1:12)

## keeps track of all the files that have issues
problem_files <- c()

for (month in months){
  print(month)
  file_names <- dir(paste(directory,month, "-2016/",sep = ''), pattern = "6.pdf")
  for (i in seq_along(file_names)) { ### for every single file in here
    print(file_names[i])
    indiana <- pdf_text(paste(directory,month,"-2016/", file_names[i], sep = ""))
    indiana <- str_to_lower(indiana)
    indiana %>% str_split('\n') %>% unlist() -> indiana
    indiana <- str_trim(indiana)
    indiana %>% str_detect("student right to know cad daily log") %>% which -> right_to_know
    if (i == 1 & month == 1 ) {
      if (!vector.is.empty(right_to_know)) {
        indiana %>% str_detect("^incident") %>% which -> incidents_indices
        indiana %>% str_detect("^date and time") %>% which -> dates_and_time_indices
        indiana %>% str_detect("^date reported") %>% which -> date_reported_indices
        
        indiana[incidents_indices] %>% 
          as_tibble() %>% 
          separate(value, into = c("value", "incident"), sep = "\\s:\\s", extra = "merge") %>% 
          separate(incident, into = c("incident", "case_number"), sep = "\\s{4,}") %>% 
          extract(case_number, "case_number", "(\\d{1,10}$)") -> incidents
        
        indiana[dates_and_time_indices] %>% 
          as_tibble() %>% 
          extract(value, "date_occurred", "(\\d{1,}/\\d{1,2}/\\d{2})", remove = F) %>% 
          extract(value, "time_occurred", "(\\d\\d:\\d\\d)") -> dates_and_time
        
        indiana[date_reported_indices] %>% 
          as_tibble() %>% 
          separate(value, into = c("date_reported", "location"), sep = "location : ") %>% 
          extract(location, "location", "(.{1,}\\s{3,})") %>% 
          extract(date_reported, "time_reported", "(\\d\\d:\\d\\d)", remove = F) %>% 
          extract(date_reported, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d\\d)") %>% 
          mutate(location = str_trim(location)) -> date_reported
        
        crime_total_r2k <- bind_cols(incidents, date_reported, dates_and_time)
      }
      
      else {
        indiana %>% str_detect("^incident") %>% which -> incidents_indices
        indiana %>% str_detect("^location|general location") %>% which -> location_indices
        indiana %>% str_detect("^date reported") %>% which -> date_reported_indices
        indiana[incidents_indices] %>% 
          as_tibble() %>% 
          separate(value, into = c("value", "incident"), sep = ":\\s", extra = "merge") %>% 
          mutate(incident = str_trim(incident)) -> incidents
        
        indiana[location_indices] %>% 
          as_tibble() %>% 
          separate(value, c("value", "location"), sep = ":") %>% 
          mutate(location = str_trim(location)) %>% 
          select(location) -> locations
        
        indiana[date_reported_indices] %>% 
          as_tibble() %>% 
          extract(value, "time_reported", "(\\d\\d:\\d\\d)", remove = F) %>% 
          extract(value, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d\\d)", remove = F) %>% 
          extract(value, "case_number", "(\\d{3,10}$)") %>% 
          mutate(date_occurred = NA, time_occurred = NA) -> date_reported
        
        crime_total <- bind_cols(incidents, locations, date_reported)
      }
    }
    else{ ## this marks the beginning of every file that is not the first file - so need to append to crime_total or crime_total_r2k
      if (!vector.is.empty(right_to_know)) {
        indiana %>% str_detect("^incident") %>% which -> incidents_indices
        indiana %>% str_detect("^date and time") %>% which -> dates_and_time_indices
        indiana %>% str_detect("^date reported") %>% which -> date_reported_indices
        
        indiana[incidents_indices] %>% 
          as_tibble() %>% 
          separate(value, into = c("value", "incident"), sep = "\\s:\\s", extra = "merge") %>% 
          separate(incident, into = c("incident", "case_number"), sep = "\\s{4,}") %>% 
          extract(case_number, "case_number", "(\\d{1,10}$)") -> incidents
        
        indiana[dates_and_time_indices] %>% 
          as_tibble() %>% 
          extract(value, "date_occurred", "(\\d{1,}/\\d{1,2}/\\d{2})", remove = F) %>% 
          extract(value, "time_occurred", "(\\d\\d:\\d\\d)") -> dates_and_time
        
        indiana[date_reported_indices] %>% 
          as_tibble() %>% 
          separate(value, into = c("date_reported", "location"), sep = "location : ") %>% 
          extract(location, "location", "(.{1,}\\s{3,})") %>% 
          extract(date_reported, "time_reported", "(\\d\\d:\\d\\d)", remove = F) %>% 
          extract(date_reported, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d\\d)") %>% 
          mutate(location = str_trim(location)) -> date_reported
        
        # if (berryFunctions::is.error(crime_total_append_r2k <- bind_cols(incidents, date_reported, locations))){
        #   problem_files <- c(problem_files,file_names[i])
        #   print(paste("skipped",file_names[i]))
        #   next
        # }
        ## did not write to the file that tells me which are the paths are are problems
        crime_total_append_r2k <- bind_cols(incidents, date_reported, dates_and_time)
        crime_total_r2k <- crime_total_r2k %>% 
          bind_rows(crime_total_append_r2k)
      }
      else {
        indiana %>% str_detect("^incident") %>% which -> incidents_indices
        indiana %>% str_detect("^location|general location") %>% which -> location_indices
        indiana %>% str_detect("^date reported") %>% which -> date_reported_indices
        indiana[incidents_indices] %>% 
          as_tibble() %>% 
          separate(value, into = c("value", "incident"), sep = ":\\s", extra = "merge") %>% 
          mutate(incident = str_trim(incident)) -> incidents
        
        indiana[location_indices] %>% 
          as_tibble() %>% 
          separate(value, c("value", "location"), sep = ":") %>% 
          mutate(location = str_trim(location)) %>% 
          select(location) -> locations
        
        indiana[date_reported_indices] %>% 
          as_tibble() %>% 
          extract(value, "time_reported", "(\\d\\d:\\d\\d)", remove = F) %>% 
          extract(value, "date_reported", "(\\d{1,2}/\\d{1,2}/\\d\\d)", remove = F) %>% 
          extract(value, "case_number", "(\\d{3,10}$)") %>% 
          mutate(date_occurred = NA, time_occurred = NA) -> date_reported
        
        
        if (exists('crime_total')) {
          if (berryFunctions::is.error(crime_total_append <- bind_cols(incidents, date_reported, locations))){
            problem_files <- c(problem_files,file_names[i])
            print(paste("skipped",file_names[i]))
            next
          }
          crime_total_append <- bind_cols(incidents, locations, date_reported)
          crime_total <- crime_total %>% 
            bind_rows(crime_total_append)
        }
        else{
          crime_total <- bind_cols(incidents, locations, date_reported)
        }
      }
      
      
    }
  }
}

crime_total_2016 <- crime_total %>% 
  select(-value) %>% 
  mutate(university = "Indiana University-Bloomington")

save(crime_total_2016, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Data/campus_daily_crime_log/Indiana University/crime_2016.rda")

write_lines(problem_files, file = paste(directory, "problem_files_2016.txt"), sep = "\n")

