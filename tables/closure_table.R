
library(tidyverse)
library(kableExtra)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv") %>% 
    filter(university %in% ifc::moratorium_schools())
}

## use options(knitr.kable.NA = '') in code chunk

closure_table <- daily_crime %>% 
  distinct(university, closure_1, closure_1_end, closure_2, closure_2_end, closure_3, closure_3_end) %>% 
  arrange(university) %>% 
  mutate(across(everything(), ~as.character(.))) %>% 
  mutate(across(everything(), ~ifelse(is.na(.), "", .)))



closure_table <- kbl(closure_table, booktabs = T,
                     col.names = c("University", "Start 1", "End 1", "Start 2", "End 2", "Start 3", "End 3"),
                     caption = "\\label{closure_table}Moratorium Dates of Each University in the Sample.") %>% 
  kable_styling(latex_options ="scale_down", font_size = 10) %>% 
  footnote(list("Universities can have multiple moratoriums in the sample period. Each moratorium date was verified by either a Fraternity and Sorority Life advisor, a news article, or a public records request. However, the first San Diego State University moratorium end date could not be directly verified by either a fraternity or sorority advisor, news article, or public record request. However, based on the following news article link, I am confident that the moratorium ended before the start of the 2015 semester. Link: https://newscenter.sdsu.edu/sdsu_newscenter/news_story.aspx?sid=75357"),
           threeparttable = T)



