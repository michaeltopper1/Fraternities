library(tidyverse)
library(modelsummary)
library(kableExtra)


cyl4 <- list(
  lm(mpg ~ hp, mtcars[mtcars$cyl == 4,]),
  lm(mpg ~ hp + drat, mtcars[mtcars$cyl == 4,])) 
cyl6 <- list(
  lm(mpg ~ hp, mtcars[mtcars$cyl == 6,]),
  lm(mpg ~ hp + drat, mtcars[mtcars$cyl == 6,])) 
cyl8 <- list(
  lm(mpg ~ hp, mtcars[mtcars$cyl == 8,]),
  lm(mpg ~ hp + drat, mtcars[mtcars$cyl == 8,])) 

tab6 <- modelsummary(cyl6, "data.frame") %>% 
  mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>% 
  filter(part == "estimates") %>% 
  select(matches("term|Model"))

tab8 <- modelsummary(cyl8, "data.frame") %>% 
  mutate(term = ifelse(statistic == "modelsummary_tmp2", "", term)) %>% 
  filter(part == "estimates") %>% 
  select(matches("term|Model"))

ar <- rbind(tab6, tab8)
attr(ar, "position") <- 7:(7 + nrow(ar))

modelsummary(cyl4, add_rows = ar) %>% 
  pack_rows("Cylinders: 4", 1, 6) %>% 
  pack_rows("Cylinders: 6", 7, 12) %>% 
  pack_rows("Cylinders: 8", 13, 18)
