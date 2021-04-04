

load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Master_data/master_ucr_ipeds_cross.rda")

library(tidyverse)
library(lfe)
library(huxtable)
library(broom)
library(gt)
library(modelsummary)
library(kableExtra)


ucr_master_nomissing <- ucr_master %>% 
  filter(university != "California State University-Northridge" 
         & university != "Miami University-Oxford") %>% 
  group_by(month, year) %>% 
  mutate(month_by_year = cur_group_id()) %>% ungroup()

ucr_master_nomissing <- ucr_master_nomissing %>% 
  group_by(university, year) %>% 
  mutate(university_by_year = cur_group_id()) %>% ungroup()

uni_only <- ucr_master_nomissing %>% 
  filter(subtype2 == "(011) Four-year university")






## no controls- bare bones
ols_inverse_nocontrol <- felm(inverse_sine_rape ~ treatment |
                                university + month_by_year| 0 |
                                university, data = ucr_master_nomissing, cmethod = "reghdfe")

## controls no missing 
ols_inverse_sat <- felm(inverse_sine_rape ~ treatment + 
                      graduation_rate  + 
                      undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                      undergrad_proportion_white  +
                      proportion_ftenroll|
                      university + month_by_year  | 0 |
                      university, data = ucr_master_nomissing, cmethod = "reghdfe")
## running with controls
ols_inverse_all <- felm(inverse_sine_rape ~ treatment + 
               graduation_rate  + 
              undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
              undergrad_proportion_white  +
              proportion_firsttime_foreign + proportion_ftenroll|
              university + month_by_year | 0 |
              university, data = ucr_master_nomissing, cmethod = "reghdfe")

## 
ols_inverse_nosat <- felm(inverse_sine_rape ~ treatment + 
                        graduation_rate  + 
                        undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                        undergrad_proportion_white +
                        proportion_firsttime_foreign + proportion_ftenroll|
                        university + month_by_year | 0 |
                        university, data = ucr_master_nomissing,
                      cmethod = "reghdfe")

## no controls- bare bones university only
ols_inverse_nocontrol_uni <- felm(inverse_sine_rape ~ treatment |
                                university + month_by_year| 0 |
                                university, data = uni_only, cmethod = "reghdfe")

## no missing 
ols_inverse_sat_uni <- felm(inverse_sine_rape ~ treatment + 
                          graduation_rate + 
                          undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                          sat_math_75_perc + sat_reading_75_perc +
                          undergrad_proportion_white
                           + proportion_ftenroll|
                          university + month_by_year | 0 |
                          university, data = uni_only, cmethod = "reghdfe") 

## running with controls university only
ols_inverse_all_uni <- felm(inverse_sine_rape ~ treatment + 
                      graduation_rate  + 
                      undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                      undergrad_proportion_white + sat_math_75_perc + sat_reading_75_perc +
                      proportion_firsttime_foreign + proportion_ftenroll|
                      university + month_by_year | 0 |
                      university, data = uni_only, cmethod = "reghdfe")

## running TWFE on restricting months to not include June, July, August
ols_inverse_nosat_uni <- felm(inverse_sine_rape ~ treatment + 
                        graduation_rate  + 
                        undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                        undergrad_proportion_white  +
                        proportion_firsttime_foreign + proportion_ftenroll|
                        university + month_by_year  | 0 |
                        university, data = uni_only ,
                      cmethod = "reghdfe")


models <- list("(1)" = ols_inverse_nocontrol, "(2)" = ols_inverse_sat, "(3)" = ols_inverse_nosat,"(4)" =  ols_inverse_all, "(1)" =  ols_inverse_nocontrol_uni, "(2)" = ols_inverse_sat_uni, "(3)" = ols_inverse_nosat_uni, "(4)" = ols_inverse_all_uni)

models <- modelsummary(models, output = 'kableExtra', stars = TRUE , coef_rename = c(
  "treatment" = "Moratorium",
  "university_enacted" = "University Enacted",
  "graduation_rate" = "Graduation Rate",
  "serving_population" = "Population Served",
  "undergrad_proportion_asian" = "Undergrad Proportion Asian",
  "undergrad_proportion_black" = "Undergrad Proportion Black",
  "undergrad_proportion_hispanic" = "Undergrad Proportion Hispanic",
  "undergrad_proportion_white" = "Undergrad Proportion White",
  "sat_math_75_perc" = "SAT Math 75th Percentile",
  "sat_reading_75_perc" = "SAT Reading 75 Percentile",
  "proportion_firsttime_foreign" = "Proportion Foreign",
  "proportion_ftenroll" = "Proportion Full-time",
  "treatment:university_enacted" = "Moratorium x University Enacted"
), title = "Effects of Fraternity Moratoriums on Inverse Hyperbolic Sine Rape")

models <- models %>% 
  add_header_above(c(" " = 1,  "Full Sample"= 4, "University Police Only" = 4)) ## goes by each column

models
