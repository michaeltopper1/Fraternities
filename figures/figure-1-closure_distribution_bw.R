## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-08
##

library(tidyverse)
library(readxl)
library(ggrepel)

closures <- read_excel("data/closure_spreadsheet_final_2019.xlsx") %>% 
  janitor::clean_names() %>% 
  filter(!is.na(date)) %>% 
  filter(university %in% ifc::moratorium_schools())

closures <- closures %>% 
  mutate(across(c(date, deadline, date2, deadline2, date3, deadline3), ~lubridate::as_date(.)))


closure_distribution <- closures %>% 
  filter(university %in% ifc::moratorium_schools()) %>% 
  mutate(university = ifelse(university == "Louisiana State University and Agricultural & Mechanical College", "Louisiana State University", university)) %>% 
  mutate(university = ifelse(university == "California Polytechnic State University-San Luis Obispo", "Cal Poly San Luis Obispo", university)) %>%
  mutate(university = ifelse(university == "University of Pittsburgh-Pittsburgh Campus", "University of Pittsburgh", university)) %>%
  mutate(university = gsub("-Main Campus", "", university)) %>% 
  mutate(unviersity = ifelse(university == "University of California-Berkeley",
                             "UC Berkeley", university)) %>% 
  mutate(university = str_replace(university, "-.+$", "")) %>% 
  mutate(university = ifelse(university == "North Carolina State University at Raleigh", "North Carolina State", university)) %>% 
  mutate(length_1 = deadline - date, length_2 = deadline2 - date2, length_3 = deadline3 - date3) %>% 
  mutate(across(starts_with("length_"), ~paste(as.character(.), "days"))) %>%
  mutate(university = fct_reorder(university, desc(university))) %>% 
  ggplot() +
  geom_segment(aes(x = university, xend = university, y = date, yend = deadline)) +
  geom_segment(aes(x = university, xend = university, y = date2, yend = deadline2)) +
  geom_segment(aes(x = university, xend = university, y = date3, yend = deadline3)) +
  geom_point( aes(x=university, y=date), color="grey", alpha = 0.7, size=3 ) +
  geom_point( aes(x=university, y=deadline), color="black",alpha = 0.7, size=3 ) +
  geom_point( aes(x=university, y=date2),color="grey", alpha = 0.7, size=3 ) +
  geom_point( aes(x=university, y=deadline2), color="black",alpha = 0.7, size=3 ) +
  geom_point( aes(x=university, y=date3), color="grey", alpha = 0.7, size=3 ) +
  geom_point( aes(x=university, y=deadline3), color="black",alpha = 0.7, size=3 ) +
  scale_y_date(date_labels="%Y",date_breaks  ="1 year") +
  coord_flip() +
  geom_text_repel(aes(x = university, y = date, label = length_1), size = 2) +
  geom_text_repel(aes(x = university, y = date2, label = length_2), size = 2) +
  geom_text_repel(aes(x = university, y = date3, label = length_3), size = 2) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 7))

ggsave(filename = "figures/michael-topper-figure-1.pdf",width = 7.5, height =4.5)  


