## Purpose of script: merge IPEDS
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-08
##

library(tidyverse)

ipeds_1 <- read_csv("Created Data/IPEDS/ipeds_1_cleaned.csv") 
ipeds_2 <- read_csv("Created Data/IPEDS/ipeds_2_cleaned.csv")

ipeds_final <- ipeds_1 %>% 
  left_join(ipeds_2) 

write_csv(ipeds_final, file = "Created Data/IPEDS/ipeds_final.csv")
            