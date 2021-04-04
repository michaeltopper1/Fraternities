

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
  mutate(inverse_sine_cleared_rape = log(tot_clr_rape_total + (tot_clr_rape_total^2 + 1)^(1/2)))

uni_only <- ucr_master_nomissing %>% 
  filter(subtype2 == "(011) Four-year university")



## no controls- bare bones
ols_cleared_nocontrols <- felm(inverse_sine_cleared_rape ~ treatment |
                                university + month_by_year | 0 |
                                university, data = ucr_master_nomissing, cmethod = "reghdfe")


## running with controls
ols_cleared_all <- felm(rape_cleared_perhundredth ~ treatment + 
                          graduation_rate  + 
                          undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                          undergrad_proportion_white + 
                          proportion_ftenroll|
                          university + month_by_year | 0 |
                          university, data = ucr_master_nomissing, cmethod = "reghdfe")

## university only- no controls
ols_cleared_nocontrols_uni <- felm(rape_cleared_perhundredth ~ treatment |
                                 university + month_by_year | 0 |
                                 university, data = uni_only, cmethod = "reghdfe")


## university only- running with controls
ols_cleared_all_uni <- felm(rape_cleared_perhundredth ~ treatment + 
                          graduation_rate  + 
                          undergrad_proportion_asian + undergrad_proportion_black + undergrad_proportion_hispanic + 
                          undergrad_proportion_white + 
                          proportion_ftenroll|
                          university + month_by_year | 0 |
                          university, data = uni_only, cmethod = "reghdfe")

indirect_test <- list("(1)" = ols_cleared_nocontrols,
                      "(2)" = ols_cleared_all,
                      "(1)" = ols_cleared_nocontrols_uni,
                      "(2)" =  ols_cleared_all_uni)

indirect_test <- modelsummary(indirect_test, output = 'kableExtra', stars = TRUE , coef_rename = c(
  "treatment" = "Moratorium",
  "university_enacted" = "University Enacted",
  "graduation_rate" = "Graduation Rate",
  "serving_population" = "Population Served",
  "undergrad_proportion_asian" = "Undergrad Proportion Asian",
  "undergrad_proportion_black" = "Undergrad Proportion Black",
  "undergrad_proportion_hispanic" = "Undergrad Proportion Hispanic",
  "undergrad_proportion_white" = "Undergrad Proportion White",
  "proportion_ftenroll" = "Proportion Full-time",
  "ori" = "Agency",
  "month_by_year" = "Month by year",
  "university" = "University"
), title = "Effects of Fraternity Moratoriums on Cleared Reports of Rape (Per 100k)")

indirect_test <- indirect_test %>% 
  add_header_above(c(" " = 1,  "Local Municipality and University Police"= 2, "University Police Only" = 2))

indirect_test
