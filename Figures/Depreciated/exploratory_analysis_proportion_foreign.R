###########################################################################
##  This file does exploratory analysis on the proportion of foreigners  ##
###########################################################################

load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Master_data/master_ucr_ipeds_cross.rda")

library(tidyverse)
library(lfe)
library(huxtable)
library(broom)
library(gt)
library(modelsummary)
library(kableExtra)

huxtable(tidy(felm(inverse_sine_rape ~ proportion_firsttime_foreign  + serving_population
                   | date + university | 0 |
                     university, data = ucr_master )))
ucr_master %>% 
  ggplot(aes(x = proportion_firsttime_foreign)) +
  geom_density(aes(fill = university), alpha = 0.3) +
  labs(x = "", y = "", fill = "University")+
  ggthemes::theme_calc()

ucr_master %>% 
  ggplot(aes(x = proportion_firsttime_foreign, y = inverse_sine_rape)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_smooth(method = "lm", formula = y~x, color = "red")