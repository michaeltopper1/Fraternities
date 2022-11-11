## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-10-25
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv") 
}

frac_ifc <- read_csv("data/fraction_ifc.csv") %>% 
  select(university, ifc_frac_updated)

ifc_quantiles <- quantile(frac_ifc$ifc_frac_updated) 
ifc_quantiles_3 <- quantile(frac_ifc$ifc_frac_updated, probs = c(0,.33,.66,1))

# adding in the ifc fractions ---------------------------------------------


daily_crime <- daily_crime %>% 
  left_join(frac_ifc) %>% 
  mutate(ifc_quantile = case_when(
    ifc_frac_updated <= ifc_quantiles[[2]] ~ 0.25,
    ifc_frac_updated <= ifc_quantiles[[3]] & ifc_frac_updated > ifc_quantiles[[2]] ~0.5,
    ifc_frac_updated <= ifc_quantiles[[4]] & ifc_frac_updated > ifc_quantiles[[3]] ~ 0.75,
    ifc_frac_updated > ifc_quantiles[[4]] ~ 1
  )) %>% 
  mutate(ifc_quantile_3 = case_when(
    ifc_frac_updated <= ifc_quantiles[[2]] ~ 0.33,
    ifc_frac_updated <= ifc_quantiles[[3]] & ifc_frac_updated > ifc_quantiles[[2]] ~0.66,
    ifc_frac_updated <= ifc_quantiles[[4]] & ifc_frac_updated > ifc_quantiles[[3]] ~ 1
  )) %>% 
  mutate(treatment_ifc = treatment * ifc_frac_updated)

daily_crime_weekends <- daily_crime_weekends %>% 
  left_join(frac_ifc) %>% 
  mutate(ifc_quantile = case_when(
    ifc_frac_updated <= ifc_quantiles[[2]] ~ 0.25,
    ifc_frac_updated <= ifc_quantiles[[3]] & ifc_frac_updated > ifc_quantiles[[2]] ~0.5,
    ifc_frac_updated <= ifc_quantiles[[4]] & ifc_frac_updated > ifc_quantiles[[3]] ~ 0.75,
    ifc_frac_updated > ifc_quantiles[[4]] ~ 1
  )) %>% 
  mutate(ifc_quantile_3 = case_when(
    ifc_frac_updated <= ifc_quantiles[[2]] ~ 0.33,
    ifc_frac_updated <= ifc_quantiles[[3]] & ifc_frac_updated > ifc_quantiles[[2]] ~0.66,
    ifc_frac_updated <= ifc_quantiles[[4]] & ifc_frac_updated > ifc_quantiles[[3]] ~ 1
  )) %>% 
  mutate(treatment_ifc = treatment * ifc_frac_updated)


daily_crime_weekdays <- daily_crime_weekdays %>% 
  left_join(frac_ifc) %>% 
  mutate(ifc_quantile = case_when(
    ifc_frac_updated <= ifc_quantiles[[2]] ~ 0.25,
    ifc_frac_updated <= ifc_quantiles[[3]] & ifc_frac_updated > ifc_quantiles[[2]] ~0.5,
    ifc_frac_updated <= ifc_quantiles[[4]] & ifc_frac_updated > ifc_quantiles[[3]] ~ 0.75,
    ifc_frac_updated > ifc_quantiles[[4]] ~ 1
  )) %>% 
  mutate(ifc_quantile_3 = case_when(
    ifc_frac_updated <= ifc_quantiles[[2]] ~ 0.33,
    ifc_frac_updated <= ifc_quantiles[[3]] & ifc_frac_updated > ifc_quantiles[[2]] ~0.66,
    ifc_frac_updated <= ifc_quantiles[[4]] & ifc_frac_updated > ifc_quantiles[[3]] ~ 1
  )) %>% 
  mutate(treatment_ifc = treatment * ifc_frac_updated)


data_list <-  list(daily_crime, daily_crime_weekends, daily_crime_weekdays)


## using this gives collinearity because it is being absorbed by the fixed effects
## we do not have dynamic estimates for this for each university. only one number
# explanatory_vars_bad <- c("treatment * ifc_frac_updated")


## new solution: use quartiles as referee #4 suggested.

## the treatment variable gives you the effect on the outcome that results from an increase in the treatment intensity. 
explanatory_vars <-  c("treatment_ifc")

fixed_effects <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")
alc_intensity <- map(data_list, ~ifc::reghdfe(.x, c("alcohol_offense_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
sex_intensity <- map(data_list, ~ifc::reghdfe(.x, c("sexual_assault_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))



# table -------------------------------------------------------------------

gof_mapping <- ifc::gof_mapping() %>%
  select(-fmt) %>%
  mutate(fmt = ifelse(raw == "nobs", 0, 3)) %>%
  add_row(raw = "mean", clean = "Mean of Dependent Variable", fmt = 3, .after = 1)

footnote_ifc <- list("Fraction IFC is the most recent number of IFC members at a university divided by the average total enrollment over 2014-2019. 
                     Note that not every university keeps record of their IFC numbers over time, and therefore,
                     the most recent number of IFC members was used in this calculation. However, based on the few universities that provided year-to-year data on their IFC populations, the total number does not substantially change over time. 
                     Standard errors shown in parenthesis are clustered by university (37 clusters) and each offense is defined as per-25000 enrolled students.
                     The interaction of In Moratorium and Fraction IFC gives a measure of moratorium intensity based on the fraction of IFC members.
                     ",
                     "* p < 0.1, ** p < 0.05, *** p < 0.01") %>% 
  map(~str_replace_all(.x, "\n", ""))


ifc_share <- panelsummary::panelsummary(alc_intensity, sex_intensity,
                           panel_labels = c("Panel A: Alcohol Offenses",
                                            "Panel B: Sexual Assaults"),
                           italic = T,
                           bold = F,
                           collapse_fe = T,
                           mean_dependent = T,
                           stars =c('*' = .1, '**' = .05, '***' = .01),
                           gof_map = gof_mapping,
                           coef_map = c("treatment_ifc" = "In Moratorium x Fraction IFC"),
                           caption = "\\label{ifc_share}The Effect of Moratoriums Interacted with IFC Share") %>% 
  add_header_above(c(" " = 1, "All Days" = 1, "Weekends" = 1, "Weekdays" = 1)) %>% 
  footnote(footnote_ifc,
           threeparttable = T)




# quantiles ---------------------------------------------------------------
# 
# 
# alc_q1 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                       filter(ifc_quantile == 0.25), c("alcohol_offense_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# alc_q2 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                       filter(ifc_quantile == 0.50), c("alcohol_offense_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# 
# alc_q3 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                       filter(ifc_quantile == 0.75), c("alcohol_offense_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# 
# alc_q4 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                       filter(ifc_quantile == 1), c("alcohol_offense_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# 
# 
# sex_q1 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                          filter(ifc_quantile == 0.25), c("sexual_assault_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# sex_q2 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                          filter(ifc_quantile == 0.50), c("sexual_assault_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# 
# sex_q3 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                          filter(ifc_quantile == 0.75), c("sexual_assault_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# 
# sex_q4 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                          filter(ifc_quantile == 1), c("sexual_assault_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# 
# 
# # threcetiles -------------------------------------------------------------
# 
# 
# alc_1 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                          filter(ifc_quantile_3 == 0.33), c("alcohol_offense_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# alc_2 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                          filter(ifc_quantile_3 == 0.66), c("alcohol_offense_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# 
# alc_3 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                          filter(ifc_quantile_3 == 1), c("alcohol_offense_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# 
# sex_1 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                         filter(ifc_quantile_3 == 0.33), c("sexual_assault_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# sex_2 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                         filter(ifc_quantile_3 == 0.66), c("sexual_assault_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# 
# sex_3 <- map(data_list, ~ifc::reghdfe(.x %>% 
#                                         filter(ifc_quantile_3 == 1), c("sexual_assault_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
# 
# 
# 
# # tables ------------------------------------------------------------------
# gof_mapping <- ifc::gof_mapping() %>% 
#   select(-fmt) %>% 
#   mutate(fmt = ifelse(raw == "nobs", 0, 3)) %>% 
#   add_row(raw = "mean", clean = "Mean of Dependent Variable", fmt = 3, .after = 1)
# 
# alc_ifc_quantile <- panelsummary::panelsummary(alc_q1, 
#                            alc_q2,
#                            alc_q3,
#                            alc_q4,
#                            panel_labels = c("Quantile 1",
#                                             "Quantile 2",
#                                             "Quantile 3",
#                                             "Quantile 4"),
#                            stars = T,
#                            italic = T,
#                            bold = F,
#                            collapse_fe = T,
#       
#                            coef_map = c("treatment" = "In Moratorium"),
#                            gof_map = gof_mapping,
#                            caption = "\\label{alc_ifc_quantile}The Effect of Moratoriums on Alcohol Offenses by Quantile of IFC Population") %>% 
#   add_header_above(c(" " = 1, "All Days", "Weekends", "Weekdays"))
# 
# sex_ifc_quantile <- panelsummary::panelsummary(sex_q1, 
#                            sex_q2,
#                            sex_q3,
#                            sex_q4,
#                            panel_labels = c("Quantile 1",
#                                             "Quantile 2",
#                                             "Quantile 3",
#                                             "Quantile 4"),
#                            stars = T,
#                            italic = T,
#                            bold = F,
#                            collapse_fe = T,
#                            coef_map = c("treatment" = "In Moratorium"),
#                            gof_map = gof_mapping,
#                            caption = "\\label{sex_ifc_quantile}The Effect of Moratoriums on  Reports of Sexual Assaults by Quantile of IFC Population") %>% 
#   add_header_above(c(" " = 1, "All Days", "Weekends", "Weekdays"))
# 
# 
# 
