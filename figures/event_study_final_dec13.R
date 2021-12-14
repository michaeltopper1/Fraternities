## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-12-13
##

library(tidyverse)
library(fixest)
library(modelsummary)

if (!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}

es_14 <- ifc::event_study_day(daily_crime, 5, 14)
es_48 <- ifc::event_study_day(daily_crime, 3, 48)


explanatory_vars_14 <- c("beta_lead_binned", "beta_lead_4", "beta_lead_3", "beta_lead_2", "treatment",
                         "beta_lag_1", "beta_lag_2", "beta_lag_3", "beta_lag_4", "beta_lag_binned")

explanatory_vars_48 <- c("beta_lead_binned", "beta_lead_2", "treatment",
                         "beta_lag_1", "beta_lag_2", "beta_lag_binned")

fixed_effects <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester")


# regs 14-day -------------------------------------------------------------

es_alc_14 <- ifc::reghdfe(es_14, "alcohol_offense_per25", explanatory_vars_14, fixed_effects, cluster = "university")


es_drug_14 <- ifc::reghdfe(es_14, "drug_offense_per25", explanatory_vars_14, fixed_effects, cluster = "university")

es_sex_14 <- ifc::reghdfe(es_14, "sexual_assault_per25", explanatory_vars_14, fixed_effects, cluster = "university")


# extracting F-statistics for pre-trends ----------------------------------

alc_14_prepvalue <- car::linearHypothesis(es_alc_14,
                      c("beta_lead_2 =0",
                        "beta_lead_3 =0",
                        "beta_lead_4 =0")) %>% broom::tidy() %>% 
  slice(2) %>% pull(3)
drug_14_prepvalue <- car::linearHypothesis(es_drug_14,
                      c("beta_lead_2 =0",
                        "beta_lead_3 =0",
                        "beta_lead_4 =0")) %>% broom::tidy()%>% 
  slice(2) %>% pull(3)

sex_14_prepvalue <- car::linearHypothesis(es_sex_14,
                      c("beta_lead_2 =0",
                        "beta_lead_3 =0",
                        "beta_lead_4 =0")) %>% broom::tidy() %>% 
  slice(2) %>% pull(3)

# car::linearHypothesis(es_alc_14,
#                       c("beta_lag_1 = 0",
#                         "beta_lag_2 =0",
#                         "beta_lag_3 =0",
#                         "beta_lag_4 =0")) %>% broom::tidy() %>% 
#   slice(2) %>% pull(3)
# car::linearHypothesis(es_drug_14,
#                       c("beta_lag_1 = 0",
#                         "beta_lag_2 =0",
#                         "beta_lag_3 =0",
#                         "beta_lag_4 =0")) %>% broom::tidy() %>% 
#   slice(2) %>% pull(3)
# car::linearHypothesis(es_sex_14,
#                       c("beta_lag_1 = 0",
#                         "beta_lag_2 =0",
#                         "beta_lag_3 =0",
#                         "beta_lag_4 =0")) %>% broom::tidy() %>% 
#   slice(2) %>% pull(3)
# regs 48 -----------------------------------------------------------------

es_alc_48 <- ifc::reghdfe(es_48, "alcohol_offense_per25", explanatory_vars_48, fixed_effects, cluster = "university")


es_drug_48 <- ifc::reghdfe(es_48, "drug_offense_per25", explanatory_vars_48, fixed_effects, cluster = "university")

es_sex_48 <- ifc::reghdfe(es_48, "sexual_assault_per25", explanatory_vars_48, fixed_effects, cluster = "university")


# graphs ------------------------------------------------------------------


es_alc_14 <- ifc::event_study_graph(es_alc_14, 5) +
  labs(x = "14 day periods before and after moratorium", y = "Coefficient Estimate and 95% Confidence Interval")

es_drug_14 <- ifc::event_study_graph(es_drug_14, 5) +
  labs(x = "14 day periods before and after moratorium", y = "Coefficient Estimate and 95% Confidence Interval")

es_sex_14 <- ifc::event_study_graph(es_sex_14, 5) +
  labs(x = "14 day periods before and after moratorium", y = "Coefficient Estimate and 95% Confidence Interval")

es_alc_48 <- ifc::event_study_graph(es_alc_48, 3) +
  labs(x = "48 day periods before and after moratorium", y = "Coefficient Estimate and 95% Confidence Interval")

es_drug_48 <- ifc::event_study_graph(es_drug_48, 3) +
  labs(x = "48 day periods before and after moratorium", y = "Coefficient Estimate and 95% Confidence Interval")

es_sex_48 <- ifc::event_study_graph(es_sex_48, 3) +
  labs(x = "48 day periods before and after moratorium", y = "Coefficient Estimate and 95% Confidence Interval")

