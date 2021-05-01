## Purpose of script: Runs all the files to get new data cleaned
##
## Author: Michael Topper
##
## Date Last Edited: 2021-03-03
##

library(tidyverse)


##IPEDS first
source("Code/Cleaning_IPEDS/clean_ipeds_race_1.R")
source("Code/Cleaning_IPEDS/clean_ipeds_ic_freq_2.R")
source("Code/Cleaning_IPEDS/clean_ipeds_grad_financial_3.R")
source("Code/Cleaning_IPEDS/clean_ipeds_sat_4.R")
source("Code/Cleaning_IPEDS/merge_ipeds.R")


## cleaning panel for daily and monthly
source("Code/Cleaning_crime_log_files/create_panel_2.R")
