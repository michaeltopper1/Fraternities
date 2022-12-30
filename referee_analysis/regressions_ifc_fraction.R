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

frac_ifc <- frac_ifc %>% 
  mutate(avg = mean(ifc_frac_updated),
         deviation_from_avg = ifc_frac_updated - avg)

ifc_quantiles <- quantile(frac_ifc$ifc_frac_updated) 
ifc_quantiles_3 <- quantile(frac_ifc$ifc_frac_updated, probs = c(0,.33,.66,1))
ifc_quantiles_6 <- quantile(frac_ifc$ifc_frac_updated, probs = c(0,.1,.2,.3, .4, .5, 1))






# adding in the ifc fractions ---------------------------------------------

create_quantiles <- . %>% 
  mutate(ifc_quantile_1 = ifelse(ifc_frac_updated <= ifc_quantiles[[2]], 1, 0),
         ifc_quantile_2 = ifelse(ifc_frac_updated <= ifc_quantiles[[3]] & ifc_frac_updated > ifc_quantiles[[2]], 1, 0),
         ifc_quantile_3 = ifelse(ifc_frac_updated <= ifc_quantiles[[4]] & ifc_frac_updated > ifc_quantiles[[3]], 1, 0),
         ifc_quantile_4 = ifelse(ifc_frac_updated > ifc_quantiles[[4]], 1, 0))

daily_crime <- daily_crime %>% 
  left_join(frac_ifc) %>% 
  create_quantiles() %>% 
  mutate(treatment_ifc = treatment * ifc_frac_updated,
         treatment_ifc_dev = treatment * deviation_from_avg)

daily_crime_weekends <- daily_crime_weekends %>% 
  left_join(frac_ifc) %>% 
  create_quantiles() %>%
  mutate(treatment_ifc = treatment * ifc_frac_updated,
         treatment_ifc_dev = treatment * deviation_from_avg)


daily_crime_weekdays <- daily_crime_weekdays %>% 
  left_join(frac_ifc) %>% 
  create_quantiles() %>%
  mutate(treatment_ifc = treatment * ifc_frac_updated,
         treatment_ifc_dev = treatment * deviation_from_avg)


data_list <-  list( daily_crime, daily_crime_weekends,daily_crime_weekdays)


## new solution: use quartiles as referee #4 suggested.

## the treatment variable gives you the effect on the outcome that results from an increase in the treatment intensity. 
explanatory_vars <-  c("treatment_ifc_dev", "treatment")

fixed_effects <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")
alc_intensity <- map(data_list, ~ifc::reghdfe(.x, c("alcohol_offense_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
sex_intensity <- map(data_list, ~ifc::reghdfe(.x, c("sexual_assault_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))



# table -------------------------------------------------------------------

gof_mapping <- ifc::gof_mapping() %>%
  select(-fmt) %>%
  mutate(fmt = ifelse(raw == "nobs", 0, 3)) %>%
  add_row(raw = "mean", clean = "Mean of Dependent Variable", fmt = 3, .after = 1)

footnote_ifc <- list("Fraction IFC is the most recent number of IFC members at a university divided by the average total enrollment over 2014-2019, centered at the mean. 
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
                           coef_map = c("treatment" = "In Moratorium",
                                        "treatment_ifc_dev" = "In Moratorium x Fraction IFC"),
                           caption = "\\label{ifc_share}The Effect of Moratoriums Interacted with IFC Share") %>% 
  add_header_above(c(" " = 1, "All Days" = 1, "Weekends" = 1, "Weekdays" = 1)) %>% 
  footnote(footnote_ifc,
           threeparttable = T)



#quantiles ---------------------------------------------------------------

data_list <-  list("All Days" = daily_crime, "Weekends" = daily_crime_weekends, "Weekdays" = daily_crime_weekdays)


alc_ifc <- map_df(data_list, ~feols(alcohol_offense_per25 ~
                           treatment:ifc_quantile_1 + treatment:ifc_quantile_2 + treatment:ifc_quantile_3 +
                           treatment:ifc_quantile_4| day_of_week + university_by_academic_year + holiday + spring_semester + game_occurred,
                         cluster = ~university, data = .x) %>% 
         broom::tidy(conf.int = T), .id = "var") %>% 
  mutate(var = factor(var, c("All Days", "Weekends", "Weekdays"))) %>% 
  ggplot(aes(term, estimate, group = var)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
  geom_point() +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_line(linetype = "dashed") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  facet_wrap(~var) +
  labs(x = "Quantile", y = "Point Estimate and 95% Confidence Interval") +
  theme_minimal()

alc_ifc_2 <- map_df(data_list, ~feols(alcohol_offense_per25 ~
                                      treatment:ifc_quantile_1 + treatment:ifc_quantile_2 + treatment:ifc_quantile_3 +
                                      treatment:ifc_quantile_4| day_of_week + university_by_academic_year + holiday + spring_semester + game_occurred,
                                    cluster = ~university, data = .x %>% 
                                      filter(university != "West Virginia University")) %>% 
                    broom::tidy(conf.int = T), .id = "var") %>% 
  mutate(var = factor(var, c("All Days", "Weekends", "Weekdays"))) %>% 
  ggplot(aes(term, estimate, group = var)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
  geom_point() +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_line(linetype = "dashed") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  facet_wrap(~var) +
  labs(x = "Quantile", y = "Point Estimate and 95% Confidence Interval") +
  theme_minimal()

sex_ifc <- map_df(data_list, ~feols(sexual_assault_per25 ~
                           treatment:ifc_quantile_1 + treatment:ifc_quantile_2 + treatment:ifc_quantile_3 +
                           treatment:ifc_quantile_4| day_of_week + university_by_academic_year + holiday + spring_semester + game_occurred,
                         cluster = ~university, data = .x) %>% 
         broom::tidy(conf.int = T),
       .id = "var") %>% 
  mutate(var = factor(var, c("All Days", "Weekends", "Weekdays"))) %>% 
  ggplot(aes(term, estimate, group = var)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3) +
  geom_point() +
  geom_hline(yintercept = 0, color = "dark red") +
  geom_line(linetype = "dashed") +
  scale_x_discrete(labels = c("Q1", "Q2", "Q3", "Q4")) +
  facet_wrap(~var) +
  labs(x = "Quantile", y = "Point Estimate on In Moratorium and 95% Confidence Interval") +
  theme_minimal()



