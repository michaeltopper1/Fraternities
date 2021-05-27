## Author: Michael Topper
##
## Date Last Edited: 2021-03-31
##

library(modelsummary)
library(tidyverse)
library(kableExtra)

## Note that this is omitting the summer months of 6, 7, and 8. 
daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv",
                        guess_max = 50000)


## param x - my exact data frame of weekly/daily crime. DO not change the names
summarystat_crime <- function(x) {
  output_frame <- x %>% 
    select(`Sexual Assault` = sexual_assault, 
           `Alcohol Offense` = alcohol_offense,
           `Drug Offense` = drug_offense,
           `Theft` = theft,
           `Robbery/Burglary` = robbery_burglary) %>% 
    summarize(across(everything(), list(Mean = ~mean(.,na.rm = T), 
                                        SD = ~sd(.,na.rm = T),
                                        Median = ~median(.,na.rm = T), 
                                        Min = ~min(.,na.rm = T), 
                                        Max = ~max(.,na.rm = T))),
              .names = "{.col}_{.fn}")  %>% select(-`.names`) %>% 
    pivot_longer(cols = everything(),
                 names_sep = "_",
                 names_to = c(" ", ".value")) 
  return(output_frame)
}

crime_daily_table <- summarystat_crime(daily_crime)
crime_daily_table_p25 <- daily_crime %>% 
  mutate(across(c(sexual_assault, alcohol_offense,
                  drug_offense, theft, robbery_burglary), ~(./total_enrollment) * 25000 )) %>% 
  summarystat_crime()

crime_table <- bind_rows(crime_daily_table, crime_daily_table_p25)



crime_table <-  kbl(crime_table, booktabs = T, digits = 2, caption = "Summary statistics for offenses during academic calendar days",
    caption.short = "Note that the summer months 6,7, and 8 are ommitted") %>% 
  pack_rows("Daily Reports", 1, 5) %>% 
  pack_rows("Daily Reports Per 25k Students", 6, 10)



