                              
## Purpose of script: merges all of the ipeds data together
##
## Author: Michael Topper
##
## Date Last Edited: 2021-02-19
##

library(tidyverse)

directory <- "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/IPEDS/unappended"
setwd(directory)
ipeds <- map(list.files(pattern = ".csv$"), ~read_csv(.))

ipeds_main <- ipeds[[1]]

ipeds_merge <- ipeds[2:4]

ipeds_final <- ipeds_main %>% 
  left_join(ipeds_merge[[1]]) %>% 
  left_join(ipeds_merge[[2]]) %>% 
  left_join(ipeds_merge[[3]])

write_csv(ipeds_final, file = "/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/IPEDS/ipeds_cleaned_appended.csv")
