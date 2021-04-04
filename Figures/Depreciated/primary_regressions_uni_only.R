
load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Master_data/master_ucr_ipeds_cross.rda")

library(tidyverse)
library(lfe)
library(broom)
library(modelsummary)
library(kableExtra)


ucr_master <- ucr_master %>% 
  filter(subtype2 == "(011) Four-year university")

ols_u <- felm(actual_rape_total ~ treatment + partyschool + university_enacted + graduation_rate + total_undergrad + undergrad_total_asian + undergrad_total_black + undergrad_total_hispanic + undergrad_total_white| ori + university + month + year | 0 | university, data = ucr_master )
#ols <- tidy(ols)
ols_log_u <- felm(log_rape ~ treatment + partyschool + university_enacted + graduation_rate + total_undergrad + undergrad_total_asian + undergrad_total_black + undergrad_total_hispanic + undergrad_total_white | ori + university + month + year | 0 | university, data = ucr_master )
#ols_log <- tidy(ols_log, conf.int = T)


## running TWFE on restricting months to not include June, July, August
ols_u_r <- felm(actual_rape_total ~ treatment + partyschool + university_enacted + graduation_rate + total_undergrad + undergrad_total_asian + undergrad_total_black + undergrad_total_hispanic + undergrad_total_white| ori + university + month + year | 0 | university, data = ucr_master %>% filter(month != 6 & month != 7 & month != 8) )
#ols_r <- tidy(ols_restrictmonth, conf.int = T)
ols_log_u_r <- felm(log_rape ~ treatment + partyschool + university_enacted + graduation_rate + total_undergrad + undergrad_total_asian + undergrad_total_black + undergrad_total_hispanic + undergrad_total_white | ori + university + month + year | 0 | university, data = ucr_master %>% filter(month != 6 & month != 7 & month != 8) )
#ols_log_r <- tidy(ols_restrictmonth_log, conf.int = T)



coef_labels = c("Treatment", "Party School",  "University Enacted", "Graduation Rate", "Total Undergraduates", "Undergrad Asian", "Undergrad Black", "Undergrad Hispanic", "Undergrad Black")
models_u <- list("Rape" = ols_u, "Log(Rape+1)" = ols_log_u, "Rape" =  ols_u_r, "Log(Rape + 1)" = ols_log_u_r)
models_u <- modelsummary(models_u, output = 'kableExtra', stars = TRUE , coef_rename = c(
  "treatment" = "Treatment",
  "university_enacted" = "University Enacted",
  "graduation_rate" = "Graduation Rate",
  "partyschool" = "Party School",
  "total_undergrad" = "Total Undergrad",
  "undergrad_total_asian" = "Undergrad Asian",
  "undergrad_total_black" = "Undergrad Black",
  "undergrad_total_hispanic" = "Undergrad Hispanic",
  "undergrad_total_white" = "Undergrad White"
), title = "Effects of Fraternity Moratoriums on Rape (University Police Only)")

models_u <- models_u %>% 
  add_header_above(c(" " = 1,  "Full Year"= 2, "Excluding Summer" = 2)) ## goes by each column
