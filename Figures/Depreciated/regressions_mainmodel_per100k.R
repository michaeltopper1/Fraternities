
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
  filter(subtype2 == "(011) Four-year university")

## no controls- bare bones
ols_nocontrol <- felm(rape_per_hundredthousand ~ treatment |
                        university + month_by_year | 0 |
                        university, data = ucr_master_nomissing, cmethod = "reghdfe")

## running TWFE on restricting months to not include June, July, August
ols_all <- felm(rape_per_hundredthousand ~ treatment + 
                  graduation_rate  + 
                  undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                  undergrad_proportion_white +
                  proportion_ftenroll|
                  university + month_by_year | 0 |
                  university, data = ucr_master_nomissing,
                  cmethod = "reghdfe")

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

model_2 <- list("(1)" = ols_nocontrol, 
               "(2)" = ols_all, 
               "(1)" = ols_inverse_nocontrol_uni,
               "(2)" =  ols_inverse_all_uni)

model_2 <- modelsummary(model_2, output = 'kableExtra', stars = TRUE , coef_rename = c(
  "treatment" = "Moratorium",
  "university_enacted" = "University Enacted",
  "graduation_rate" = "Graduation Rate",
  "serving_population" = "Population Served",
  "undergrad_proportion_asian" = "Undergrad Proportion Asian",
  "undergrad_proportion_black" = "Undergrad Proportion Black",
  "undergrad_proportion_hispanic" = "Undergrad Proportion Hispanic",
  "undergrad_proportion_white" = "Undergrad Proportion White",
  "proportion_ftenroll" = "Proportion Full-time"
), title = "Effects of Fraternity Moratoriums on Rape (per 100k)")

model_2 <- model_2 %>% 
  add_header_above(c(" " = 1,  "Local Municipality and University Police"= 2, "University Police Only" = 2)) ## goes by each column
