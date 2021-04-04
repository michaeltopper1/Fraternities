load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Master_data/master_ucr_ipeds_cross.rda")

library(tidyverse)
library(lfe)
library(huxtable)
library(broom)
library(gt)
library(modelsummary)
library(kableExtra)

uni_only <- ucr_master %>% 
  filter(subtype2 == "(011) Four-year university")

## no controls- bare bones
ols_nocontrol <- felm(per_capita_rape ~ treatment*university_enacted |
                                university + date | 0 |
                                university, data = ucr_master, cmethod = "reghdfe")

## controls no missing 
ols_sat <- felm(per_capita_rape ~ treatment*university_enacted + 
                               graduation_rate  + 
                               undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic +
                               sat_math_75_perc + sat_reading_75_perc +
                               undergrad_proportion_white  +
                               proportion_ftenroll|
                               university + date | 0 |
                               university, data = ucr_master, cmethod = "reghdfe")
## running with controls
ols_nosat <- felm(per_capita_rape ~ treatment*university_enacted + 
                      graduation_rate  + 
                      undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                      undergrad_proportion_white + 
                      proportion_firsttime_foreign + proportion_ftenroll|
                      university + date | 0 |
                      university, data = ucr_master, cmethod = "reghdfe")

## running TWFE on restricting months to not include June, July, August
ols_all <- felm(per_capita_rape ~ treatment*university_enacted + 
                               graduation_rate  + 
                               undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                               undergrad_proportion_white + sat_math_75_perc + sat_reading_75_perc +
                               proportion_firsttime_foreign + proportion_ftenroll|
                               university + date | 0 |
                               university, data = ucr_master,
                             cmethod = "reghdfe")

## no controls- bare bones university only
ols_nocontrol_uni <- felm(per_capita_rape ~ treatment*university_enacted |
                                    university + date | 0 |
                                    university, data = uni_only, cmethod = "reghdfe")

## no missing 
ols_sat_uni <- felm(per_capita_rape ~ treatment*university_enacted + 
                                   graduation_rate +   
                                   undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                                   undergrad_proportion_white + sat_math_75_perc + sat_reading_75_perc +
                                   + proportion_ftenroll|
                                   university + date | 0 |
                                   university, data = uni_only, cmethod = "reghdfe") 

## running with controls university only
ols_nosat_uni <- felm(per_capita_rape ~ treatment*university_enacted + 
                          graduation_rate  + 
                          undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                          undergrad_proportion_white + 
                          proportion_firsttime_foreign + proportion_ftenroll|
                          university + date | 0 |
                          university, data = uni_only, cmethod = "reghdfe")

## running TWFE on restricting months to not include June, July, August
ols_all_uni <- felm(per_capita_rape ~ treatment*university_enacted + 
                                   graduation_rate  + 
                                   undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                                   undergrad_proportion_white + sat_math_75_perc + sat_reading_75_perc +
                                   proportion_firsttime_foreign + proportion_ftenroll|
                                   university + date | 0 |
                                   university, data = uni_only,
                                 cmethod = "reghdfe")


model_2 <- list("(1)" = ols_nocontrol, "(2)" = ols_sat, "(3)" = ols_nosat,"(4)" =  ols_all, "(1)" =  ols_nocontrol_uni, "(2)" = ols_sat_uni, "(3)" = ols_nosat_uni, "(4)" = ols_all_uni)

model_2 <- modelsummary(model_2, output = 'kableExtra', stars = TRUE , fmt = '%.3e', coef_rename = c(
  "treatment" = "Moratorium",
  "university_enacted" = "University Enacted",
  "graduation_rate" = "Graduation Rate",
  "undergrad_proportion_asian" = "Undergrad Proportion Asian",
  "undergrad_proportion_black" = "Undergrad Proportion Black",
  "undergrad_proportion_hispanic" = "Undergrad Proportion Hispanic",
  "undergrad_proportion_white" = "Undergrad Proportion White",
  "sat_math_75_perc" = "SAT Math 75th Percentile",
  "sat_reading_75_perc" = "SAT Reading 75 Percentile",
  "proportion_firsttime_foreign" = "Proportion Foreign",
  "proportion_ftenroll" = "Proportion Full-time",
  "treatment:university_enacted" = "Moratorium x University Enacted"
), title = "Effects of Fraternity Moratoriums on Per-capita Rape")

model_2 <- model_2 %>% 
  add_header_above(c(" " = 1,  "Full Sample"= 4, "University Police Only" = 4)) ## goes by each column
