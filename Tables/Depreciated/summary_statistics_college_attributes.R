
################################################################################################################################
##  This file creates the summary statistics for the schools using the IPEDS data. To be updated using the FOIA stuff later.  ##
################################################################################################################################


library(tidyverse)


load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Master_data/master_ucr_ipeds_cross.rda")


ipeds_columns <- ucr_master %>% 
  select(university, ori, month, year, total_undergrad, total_undergrad_men, 
         total_undergrad_women, starts_with("undergrad_total"), graduation_rate, level_of_institution, institutional_category, control_of_institution, partyschool, actual_rape_total)

ipeds_columns <- ipeds_columns %>% 
  mutate_at(vars(starts_with("undergrad_total")), funs(. / total_undergrad)) %>% 
  rename_at(vars(starts_with("undergrad_total")), ~gsub("undergrad_total", "undergrad_percent", .)) %>% 
  select(-ends_with("men"), -ends_with("women"))

averages <- ipeds_columns %>% 
  group_by(university, year) %>% 
  summarize_at(vars( total_undergrad, graduation_rate, starts_with("undergrad_percent")), mean) %>% 
  summarize_at(vars( total_undergrad, graduation_rate, starts_with("undergrad_percent")), mean) %>% 
  summarize_at(vars(total_undergrad, graduation_rate, starts_with("undergrad_percent")), list(mean, max, min, sd)) %>% 
  rename_all(~gsub("fn1", "mean", .)) %>% 
  rename_all( ~gsub("fn2", "max",.)) %>% 
  rename_all(~gsub("fn3", "min",.)) %>% 
  rename_all(~gsub("fn4", "sd",.))

names <- c("Mean", "Max", "Min", "Std Dev")
rownames_table <- c("Undergraduate Enrollment", "Graduation Rate", "Percent Asian", "Percent Black", "Percent Hispanic",
                    "Percent White")

rape <- averages %>% 
  select(starts_with("actual_")) %>% 
  rename_at(vars(colnames(.)), ~names)


total_undergrad_enrol <- averages %>% 
  select(starts_with("total_undergrad")) %>% 
  rename_at(vars(colnames(.)), ~ names)

graduation_rate <- averages %>% 
  select(starts_with("graduation_rate"))  %>% 
  rename_at(vars(colnames(.)), ~ names)

percent_white <- averages %>% 
  select(starts_with("undergrad_percent_white"))  %>% 
  rename_at(vars(colnames(.)), ~ names)

percent_black <- averages %>% 
  select(starts_with("undergrad_percent_black"))  %>% 
  rename_at(vars(colnames(.)), ~ names)

percent_asian <- averages %>% 
  select(starts_with("undergrad_percent_asian"))  %>% 
  rename_at(vars(colnames(.)), ~ names)

percent_hispanic <- averages %>% 
  select(starts_with("undergrad_percent_hispanic"))  %>% 
  rename_at(vars(colnames(.)), ~ names)

summary_stats <- bind_rows(rape, total_undergrad_enrol, graduation_rate, percent_asian, percent_black, percent_hispanic, percent_white)

summary_stats <- bind_cols(summary_stats, " " =rownames_table)

summary_stats <- summary_stats %>% 
  relocate(` `) %>% 
  mutate(across(where(is.numeric), ~round(.,3)))


summary_stats
