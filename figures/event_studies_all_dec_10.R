## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-12-06
##

library(tidyverse)


# nonstaggered ------------------------------------------------------------

es_7 <- ifc::event_study_day(daily_crime, 8, 7)
es_14 <- ifc::event_study_day(daily_crime, 5, 14)
es_14_r <- ifc::event_study_day(daily_crime %>% 
                                  filter(university %in% short_moratorium_schools_fullsem), 5, 14)
es_48 <- ifc::event_study_day(daily_crime, 3, 48 )
es_48_r <- ifc::event_study_day(daily_crime %>% 
                                  filter(university %in% short_moratorium_schools_fullsem), 3, 48 )


# staggered ---------------------------------------------------------------

es_7_s <- ifc::event_study_stagger(daily_crime, 8, 7)
es_14_s <- ifc::event_study_stagger(daily_crime, 5, 14)
es_48_s <- ifc::event_study_stagger(daily_crime, 3, 48 )


# controls/explanatory ----------------------------------------------------

explanatory_vars_7 <- c("beta_lead_binned", "beta_lead_7", "beta_lead_6", "beta_lead_5", "beta_lead_4", "beta_lead_3", "beta_lead_2", "treatment",
                           "beta_lag_1", "beta_lag_2", "beta_lag_3", "beta_lag_4", "beta_lag_5", "beta_lag_6", "beta_lag_7", "beta_lag_binned")
explanatory_vars_14 <- c("beta_lead_binned", "beta_lead_4", "beta_lead_3", "beta_lead_2", "treatment",
                            "beta_lag_1", "beta_lag_2", "beta_lag_3", "beta_lag_4", "beta_lag_binned")

explanatory_vars_48 <- c("beta_lead_binned", "beta_lead_2", "treatment",
                             "beta_lag_1", "beta_lag_2", "beta_lag_binned")


explanatory_vars_7_s <- c("beta_lead_binned", "beta_lead_7", "beta_lead_6", "beta_lead_5", "beta_lead_4", "beta_lead_3", "beta_lead_2", "beta_0",
                           "beta_lag_1", "beta_lag_2", "beta_lag_3", "beta_lag_4", "beta_lag_5", "beta_lag_6", "beta_lag_7", "beta_lag_binned")
explanatory_vars_14_s <- c("beta_lead_binned", "beta_lead_4", "beta_lead_3", "beta_lead_2", "beta_0",
                           "beta_lag_1", "beta_lag_2", "beta_lag_3", "beta_lag_4", "beta_lag_binned")

explanatory_vars_48_s<- c("beta_lead_binned","beta_lead_2", "beta_0",
                             "beta_lag_1", "beta_lag_2", "beta_lag_binned")


# regs 7 day --------------------------------------------------------------------

es_alc_7 <- map(daily_fixed_effects, ~ifc::reghdfe(es_7, "alcohol_offense_per25", explanatory_vars_7, ., cluster = "university"))


es_drug_7 <- map(daily_fixed_effects, ~ifc::reghdfe(es_7, "drug_offense_per25", explanatory_vars_7, ., cluster = "university"))

es_sex_7 <- map(daily_fixed_effects, ~ifc::reghdfe(es_7, "sexual_assault_per25", explanatory_vars_7, ., cluster = "university"))


# regs 14-day -------------------------------------------------------------

es_alc_14 <- map(daily_fixed_effects, ~ifc::reghdfe(es_14, "alcohol_offense_per25", explanatory_vars_14, ., cluster = "university"))


es_drug_14 <- map(daily_fixed_effects, ~ifc::reghdfe(es_14, "drug_offense_per25", explanatory_vars_14, ., cluster = "university"))

es_sex_14 <- map(daily_fixed_effects, ~ifc::reghdfe(es_14, "sexual_assault_per25", explanatory_vars_14, ., cluster = "university"))


es_alc_14_r <- map(daily_fixed_effects, ~ifc::reghdfe(es_14_r, "alcohol_offense_per25", explanatory_vars_14, ., cluster = "university"))


es_drug_14_r <- map(daily_fixed_effects, ~ifc::reghdfe(es_14_r, "drug_offense_per25", explanatory_vars_14, ., cluster = "university"))

es_sex_14_r <- map(daily_fixed_effects, ~ifc::reghdfe(es_14_r, "sexual_assault_per25", explanatory_vars_14, ., cluster = "university"))


# regs 48 -----------------------------------------------------------------


es_alc_48 <- map(daily_fixed_effects, ~ifc::reghdfe(es_48, "alcohol_offense_per25", explanatory_vars_48, ., cluster = "university"))


es_drug_48 <- map(daily_fixed_effects , ~ifc::reghdfe(es_48, "drug_offense_per25", explanatory_vars_48, ., cluster = "university"))

es_sex_48 <- map(daily_fixed_effects, ~ifc::reghdfe(es_48, "sexual_assault_per25", explanatory_vars_48, ., cluster = "university"))

es_alc_48_r <- map(daily_fixed_effects, ~ifc::reghdfe(es_48_r, "alcohol_offense_per25", explanatory_vars_48, ., cluster = "university"))


es_drug_48_r <- map(daily_fixed_effects , ~ifc::reghdfe(es_48_r, "drug_offense_per25", explanatory_vars_48, ., cluster = "university"))

es_sex_48_r <- map(daily_fixed_effects, ~ifc::reghdfe(es_48_r, "sexual_assault_per25", explanatory_vars_48, ., cluster = "university"))


# 
# # regs 7 stagger ----------------------------------------------------------
# 
# es_alc_7_s <- map(daily_fixed_effects, ~ifc::reghdfe(es_7_s, "alcohol_offense_per25", explanatory_vars_7_s, ., cluster = "university"))
# 
# 
# es_drug_7_s <- map(daily_fixed_effects, ~ifc::reghdfe(es_7_s, "drug_offense_per25", explanatory_vars_7_s, ., cluster = "university"))
# 
# es_sex_7_s <- map(daily_fixed_effects, ~ifc::reghdfe(es_7_s, "sexual_assault_per25", explanatory_vars_7_s, ., cluster = "university"))
# 
# 
# # regs 14 stagger ---------------------------------------------------------
# 
# 
# es_alc_14_s <- map(daily_fixed_effects, ~ifc::reghdfe(es_14_s, "alcohol_offense_per25", explanatory_vars_14_s, ., cluster = "university"))
# 
# 
# es_drug_14_s <- map(daily_fixed_effects, ~ifc::reghdfe(es_14_s, "drug_offense_per25", explanatory_vars_14_s, ., cluster = "university"))
# 
# es_sex_14_s <- map(daily_fixed_effects, ~ifc::reghdfe(es_14_s, "sexual_assault_per25", explanatory_vars_14_s, ., cluster = "university"))
# 
# 
# # regs 48 stagger ---------------------------------------------------------
# 
# es_alc_48_s <- map(daily_fixed_effects, ~ifc::reghdfe(es_48_s, "alcohol_offense_per25", explanatory_vars_48_s, ., cluster = "university"))
# 
# 
# es_drug_48_s <- map(daily_fixed_effects, ~ifc::reghdfe(es_48_s, "drug_offense_per25", explanatory_vars_48_s, ., cluster = "university"))
# 
# es_sex_48_s <- map(daily_fixed_effects, ~ifc::reghdfe(es_48_s, "sexual_assault_per25", explanatory_vars_48_s, ., cluster = "university"))
# 
# 
# 
