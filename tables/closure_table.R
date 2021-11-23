
library(tidyverse)
library(kableExtra)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv") %>% 
    filter(university %in% ifc::moratorium_schools())
}

## use options(knitr.kable.NA = '') in code chunk

closure_table <- daily_crime %>% 
  distinct(university, closure_1, closure_1_end, closure_2, closure_2_end) %>% 
  arrange(university)

closure_table <- kbl(closure_table, booktabs = T,
    col.names = c("University", "Moratorium 1 Start", "Moratorium 1 End", "Moratorium 2 Start", "Moratorium 2 End"),
    caption = "\\label{closure_table}Moratorium dates of the 38 universities in the sample. Universities can have multiple moratoriums in the sample period.") %>% 
  kable_styling(latex_options = "scale_down")
