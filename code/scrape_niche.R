## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-11-02
##

library(tidyverse)
library(rvest)

page_one <- read_html("https://www.niche.com/colleges/search/best-greek-life-colleges/")

uni_1 <- page_one %>% 
  html_elements(".search-result__title-wrapper") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(university = value) %>%
  filter(!university %in% c("Stanbridge University - Riverside", "Mount Saint Mary's University Los Angeles", "Crafton Hills College"))

page_two <- read_html("https://www.niche.com/colleges/search/best-greek-life-colleges/?page=2")

uni_2 <- page_two %>% 
  html_elements(".search-result__title-wrapper") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(university = value) %>%
  filter(!university %in% c("California State University - Northridge",
                            "Dominican University of California",
                            "Crafton Hills College"))

page_three <- read_html("https://www.niche.com/colleges/search/best-greek-life-colleges/?page=3")

uni_3 <- page_three %>% 
  html_elements(".search-result__title-wrapper") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(university = value) %>% 
  filter(!university %in% c("SAE Institute")) %>% 
  slice(-c(26,27)) 

page_four <- read_html("https://www.niche.com/colleges/search/best-greek-life-colleges/?page=4")

uni_4 <- page_four %>% 
  html_elements(".search-result__title-wrapper") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(university = value) %>% 
  filter(!university %in% c("University of the Pacific")) %>% 
  slice(-c(26,27)) 

top_100_2022 <- bind_rows(uni_1, uni_2, uni_3, uni_4)


page_five <- read_html("https://www.niche.com/colleges/search/best-greek-life-colleges/?page=5")

uni_5 <- page_five %>% 
  html_elements(".search-result__title-wrapper") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(university = value) %>%
  slice(-c(4, 27,28)) 


page_six <- read_html("https://www.niche.com/colleges/search/best-greek-life-colleges/?page=6")

uni_6 <- page_six %>% 
  html_elements(".search-result__title-wrapper") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(university = value) %>%
  slice(-c(4, 27,28)) 

page_seven <- read_html("https://www.niche.com/colleges/search/best-greek-life-colleges/?page=7")

uni_7 <- page_seven %>% 
  html_elements(".search-result__title-wrapper") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(university = value) %>%
  slice(-c(4, 27,28)) 

page_eight <- read_html("https://www.niche.com/colleges/search/best-greek-life-colleges/?page=8")

uni_8 <- page_eight %>% 
  html_elements(".search-result__title-wrapper") %>% 
  html_text2() %>% 
  as_tibble() %>% 
  rename(university = value) %>%
  slice(-c(4, 27,28)) 

pages <- 20
link <- "https://www.niche.com/colleges/search/best-greek-life-colleges/?page="
for (i in 1:pages){
  print(i)
  Sys.sleep(10)
  page <- read_html(paste0(link,1))
  if (i == 1) {
    top_universities <- page %>% 
      html_elements(".search-result__title-wrapper") %>% 
      html_text2() %>% 
      as_tibble() %>% 
      rename(university = value) %>%
      slice(-c(4, 27,28))
  }
  else {
    top_universities_add <- page %>% 
      html_elements(".search-result__title-wrapper") %>% 
      html_text2() %>% 
      as_tibble() %>% 
      rename(university = value) %>%
      slice(-c(4, 27,28))
    top_universities <- top_universities %>% 
      bind_rows(top_universities_add)
  }
}
httr::GET("https://www.niche.com/colleges/search/best-greek-life-colleges/?page=1") %>% 
  read_html()
polite::bow("https://www.niche.com/colleges/search/best-greek-life-colleges/?page=1")
write_csv(top_100_2022, "data/top_100_niche_frat.csv")
moratorium_schools <- ifc::moratorium_schools()
  