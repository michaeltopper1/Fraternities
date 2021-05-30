## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-05-16
##

library(tidyverse)
library(fixest)
library(kableExtra)
library(modelsummary)

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv")

daily_crime <- daily_crime %>% 
  mutate(closure_1_lead = closure_1 - days(7)) %>% 
  relocate(closure_1, closure_1_lead) %>% 
  mutate(lead_1 = ifelse(date >= closure_1_lead & date < closure_1 & !(university %in% ifc::untreated_universities()), 1, 0))

daily_crime <- daily_crime %>% 
  mutate(closure_2_lead = closure_2 - days(7)) %>% 
  relocate(closure_2, closure_2_lead) %>% 
  mutate(lead_2 = ifelse(date >= closure_2_lead & date < closure_2 & !(university %in% ifc::untreated_universities()), 1, 0))

daily_crime <- daily_crime %>% 
  mutate(lead = ifelse(lead_1 == 1 | lead_2 == 1, 1,0)) %>% 
  mutate(lead = ifelse(is.na(lead), 0, lead))

sex <- daily_crime %>% 
  feols(sexual_assault_per25 ~ treatment + graduation_rate_total_cohort + 
          frac_undergrad_asian + frac_undergrad_hispanic_latino + frac_undergrad_black|
          uni_semester + weekday, cluster = ~university, data = .)

alc <- daily_crime %>% 
  feols(alcohol_offense_per25 ~ treatment + graduation_rate_total_cohort + 
          frac_undergrad_asian + frac_undergrad_hispanic_latino + frac_undergrad_black|
          uni_semester + weekday, cluster = ~university, data = .)

drug <- daily_crime %>% 
  feols(drug_offense_per25 ~ treatment + graduation_rate_total_cohort + 
          frac_undergrad_asian + frac_undergrad_hispanic_latino + frac_undergrad_black|
          uni_semester + weekday, cluster = ~university, data = .)

robbery <- daily_crime %>% 
  feols(robbery_burglary_per25 ~ treatment + graduation_rate_total_cohort + 
          frac_undergrad_asian + frac_undergrad_hispanic_latino + frac_undergrad_black|
          uni_semester + weekday, cluster = ~university, data = .)
  

sex_p <- daily_crime %>% 
  fepois(sexual_assault ~ treatment + graduation_rate_total_cohort + 
          frac_undergrad_asian + frac_undergrad_hispanic_latino + frac_undergrad_black|
          uni_semester + weekday, cluster = ~university, data = .)

alc_p <- daily_crime %>% 
  fepois(alcohol_offense ~ treatment + graduation_rate_total_cohort + 
          frac_undergrad_asian + frac_undergrad_hispanic_latino + frac_undergrad_black|
          uni_semester + weekday, cluster = ~university, data = .)

drug_p <- daily_crime %>% 
  fepois(drug_offense ~ treatment + graduation_rate_total_cohort + 
          frac_undergrad_asian + frac_undergrad_hispanic_latino + frac_undergrad_black|
          uni_semester + weekday, cluster = ~university, data = .)

robbery_p <- daily_crime %>% 
  fepois(robbery_burglary ~ treatment + graduation_rate_total_cohort + 
          frac_undergrad_asian + frac_undergrad_hispanic_latino + frac_undergrad_black|
          uni_semester + weekday, cluster = ~university, data = .)

main_regs <- list(
  "Sexual Assault (per 25k)" = sex,
  "Alcohol Offense (per 25k)" = alc,
  "Drug Offense (per 25k)" = drug,
  "Robbery (per 25k)" = robbery,
  "Sexual Assault" = sex_p,
  "Alcohol Offense" = alc_p,
  "Drug Offense" = drug_p,
  "Robbery" = robbery_p
)

main_regs <- modelsummary(main_regs, stars = T, gof_omit = 'DF|Deviance|AIC|BIC|Log|R2 Within',
             coef_map = c("treatment" = "Moratorium",
                          "ftime_total_undergrad" = "Fraction Full-time Undergrad",
                          "frac_undergrad_black" = "Fraction Undergrad Black",
                          "frac_undergrad_asian" = "Fraction Undergrad Asian",
                          "frac_undergrad_hispanic_latino" = "Fraction Undergrad Hispanic",
                          "graduation_rate_total_cohort" = "Graduation Rate"),
             title = "Effect of Fraternity Moratoria on Crime",
             notes = "Poisson regressions are in counts of offenses.
             Fixed effects are university-by-semester and weekday.") %>% 
  add_header_above(c(" " = 1, "OLS" = 4, "Poisson" = 4))
