load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Master_data/master_ucr_ipeds_cross.rda")

library(tidyverse)
library(lfe)
library(huxtable)
library(broom)
library(gt)
library(modelsummary)
library(kableExtra)


## these are the universities that I do not yet have a closure date for. 
ucr_master_nomissing <- ucr_master %>% 
  filter(university != "California State University-Northridge" 
         & university != "Miami University-Oxford") 

uni_only <- ucr_master %>% 
  filter(subtype2 == "(011) Four-year university") %>% 
  filter(state_abb != "FL") %>% 
  filter(ori7 != "CA00127") %>% 
  filter(ori7 != "IL0167D") %>% 
  filter(ori7 != "MA009TU") %>% 
  filter(ori7 != "PA05452") %>% 
  filter(ori != "SC01017")

## no controls- bare bones
ols_nocontrol_uni <- felm(rape_per_hundredthousand ~ treatment |
                        university + month_by_year | 0 |
                        university, data = uni_only, cmethod = "reghdfe")

## running TWFE on restricting months to not include June, July, August
ols_all_uni <- felm(rape_per_hundredthousand ~ treatment + 
                  graduation_rate  + 
                  undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                  undergrad_proportion_white +
                  proportion_ftenroll|
                  university + month_by_year | 0 |
                  university, data = uni_only,
                cmethod = "reghdfe")

## no controls- bare bones university only
ols_inverse_nocontrol_uni <- felm(inverse_sine_rape ~ treatment |
                                    university + month_by_year| 0 |
                                    university, data = uni_only, cmethod = "reghdfe")

## running with controls university only
ols_inverse_all_uni <- felm(inverse_sine_rape ~ treatment + 
                              graduation_rate  + 
                              undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                              undergrad_proportion_white +
                              proportion_ftenroll|
                              university + month_by_year | 0 |
                              university, data = uni_only, cmethod = "reghdfe")


models_uni <- list("(1)" = ols_nocontrol_uni, 
               "(2)" = ols_all_uni, 
               "(1)" = ols_inverse_nocontrol_uni,
               "(2)" =  ols_inverse_all_uni)

models_uni <- modelsummary(models_uni, output = 'kableExtra', stars = TRUE , coef_rename = c(
  "treatment" = "Moratorium",
  "university_enacted" = "University Enacted",
  "graduation_rate" = "Graduation Rate",
  "serving_population" = "Population Served",
  "undergrad_proportion_asian" = "Undergrad Proportion Asian",
  "undergrad_proportion_black" = "Undergrad Proportion Black",
  "undergrad_proportion_hispanic" = "Undergrad Proportion Hispanic",
  "undergrad_proportion_white" = "Undergrad Proportion White",
  "proportion_ftenroll" = "Proportion Full-time"
), title = "Effects of Fraternity Moratoriums on Reports of Rape")

models_uni <- models_uni %>% 
  add_header_above(c(" " = 1,  "Reports of Rape (Per 100k)" = 2, "IHS of Reports of Rape" = 2)) ## goes by each column











