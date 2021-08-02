
library(tidyverse)
library(kableExtra)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv")
}

## use options(knitr.kable.NA = '') in code chunk

closure_table <- daily_crime %>% 
  distinct(university, closure_1, closure_1_end, closure_2, closure_2_end) 