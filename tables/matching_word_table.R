## Purpose of script: Gives matching word table and top categories for each match figure.
##
## Author: Michael Topper
##
## Date Last Edited: 2021-4-30
##

library(tidyverse)
library(glue)
library(ifc)
library(kableExtra)
library(tidytext)

## getting in the data that I matched with 
# source("Code/Cleaning_crime_log_files/append_daily_crime_logs_1.R")


# identifier - pull from append_daily_crime_logs_1 ------------------------

appended_crime_logs <-  read_csv("created_data/xmaster_data/appended_crime_logs.csv") %>% 
  filter(university %in% ifc::moratorium_schools())

alcohol_identifiers <- "alcohol|dwi|intox|drink|dui|drunk|liquor|driving under the influence|dip|abcc|underage|dwi|underage|pula|owi|mip|under age|beer|wine|booze|minor in possession|ovi" ## got rid of disorderly conduct.
sexual_assault_identifiers <- "sex|rape|fondling|fondle" 
drug_identifiers <- "drug|narcotic|marijuana|heroin|overdose|cocaine|controlled substance"
theft_identifiers <- "larceny|theft|shoplifting|pocket-picking|steal|shop lifting" ##using nibrs
robbery_burglary_identifiers <- "robbery|burglary|unlawful entry|breaking and entering"
alcohol_identifiers_strict <- "alcohol|dwi|intox|drink|dui|drunk|liquor|driving under the influence|dip|abcc|underage|beverage|dwi|underage|container|pula|owi|mip|under age|minor in possession|ovi" ## getting rid of possesion
noise_violation_identifier <- "noise|loud"
rape_identifier <- "rape"

## table of words to match on
alcohol_words <- alcohol_identifiers %>% 
  str_replace_all("\\|", ", ")
sexual_assault_words <-  sexual_assault_identifiers %>% 
  str_replace_all("\\|", ", ")
drug_offense_words <-  drug_identifiers %>% 
  str_replace_all("\\|", ", ")
robbery_burglary_words <- robbery_burglary_identifiers %>% 
  str_replace_all("\\|", ", ")
theft_words <- theft_identifiers %>% 
  str_replace_all("\\|", ", ")
words <- list(alcohol_words,sexual_assault_words)  %>% 
  unlist()
categories <- c("Alcohol Offense","Sexual Assault")
matching_table <- tibble("Outcome" = categories, "Words to Match" = words)

matching_table <- kable(matching_table, booktabs = T,
                        caption = "\\label{matching_table}Words and Phrases used to Pattern Match on Offenses of Interest") %>%
  # column_spec(1, bold = T, color = "blue") %>%
  kable_styling(latex_options = "HOLD_position", font_size = 11) %>% 
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "30em") %>% 
  # column_spec(1, width = "8cm") %>% 
  # column_spec(2, width = "4cm") %>% 
  footnote(list("The second column represents a portion of an incident's description to pattern match on. Words for alcohol violations and sexual assaults are found by reading each university's dataset for common words within incident descriptions. For example, the word `sex' will match on `sexual assault' and `sex offense' since `sex' appears in each of these descriptions. Notably, this method likely undercounts the true number of violations in each police department's Daily Crime Log due to spelling errors. As a demonstration, the word `alcohol' may be written as `aclohol' which this matching process will not include. Some notable abbreviations include the following:",
                "`dwi' is an abbreviation for `driving while intoxicated'.",
                "`dip' is an abbrevation for `drunk in public'.",
                "`abcc' is an abbreviation for `alcohol beverage control comission'.",
                "`pula' is an abbrevation for `person under legal age'.",
                "`owi' is an abbreviation for `operating while intoxicated'.",
                "`mip' is an abbreviation for `minor in possesion'.",
                "`ovi' is an abbreivation for `operating vehicle intoxicated'."), threeparttable = T)




top_sexual_assault <- appended_crime_logs %>% 
  filter(sexual_assault == 1) %>% 
  mutate(total = n(), .before = 1) %>% 
  count(incident,total,  sort = T) %>% 
  head(15) %>% 
  rowwise() %>% 
  mutate(fraction = n/total, offense = "Sexual Assault") %>% 
  select(incident, fraction, offense)

top_alcohol_offense <- appended_crime_logs %>% 
  filter(alcohol_offense == 1) %>% 
  mutate(total = n(), .before = 1) %>% 
  count(incident,total,  sort = T) %>% 
  head(15) %>% 
  rowwise() %>% 
  mutate(fraction = n/total, offense = "Alcohol Offense") %>% 
  select(incident, fraction, offense) 


top_categories <- bind_rows(top_alcohol_offense,top_sexual_assault) 

top_categories <- top_categories %>% 
  extract(incident, "incident", regex = "(.{1,47})") %>% 
  ggplot(aes(reorder(incident, fraction), fraction)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~offense, scales = "free_y") +
  labs(x = "Incident Description", y = "Fraction of Corresponding Offense") +
  geom_text(aes(label = round(fraction, 2)), nudge_y = 0.009, size = 10) +
  theme_minimal() +
  theme(legend.position ="bottom",
        text = element_text(size = 45)) +
  ggsci::scale_fill_npg()





