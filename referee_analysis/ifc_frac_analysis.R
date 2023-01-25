library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)
options(modelsummary_model_labels="model")



if (!exists("daily_crime")){
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}
if (!exists("daily_crime_weekends")){
  daily_crime_weekends <- read_csv("created_data/xmaster_data/daily_panel_weekends.csv")
}

if (!exists("daily_crime_weekdays")){
  daily_crime_weekdays <- read_csv("created_data/xmaster_data/daily_panel_weekdays.csv") 
}

ipeds <- read_csv("created_data/ipeds/ipeds_final.csv")
ifc <- read_csv("data/fraction_ifc.csv")

# ## Run this code to make it the max across all years
# ifc <- ifc %>% 
#   mutate(max_ifc = pmax(spring_ifc_2014,spring_ifc_2015,
#                    spring_ifc_2016, spring_ifc_2017,
#                    spring_ifc_2018, spring_ifc_2019, 
#                    spring_ifc_2020,
#                    fall_ifc_2014, fall_ifc_2015,
#                    fall_ifc_2016, fall_ifc_2017,
#                    fall_ifc_2018, fall_ifc_2019,
#                    fall_ifc_2020,
#                    most_recent, na.rm = T), .before = 1) %>% 
#   mutate(across(matches("^spring|^fall"), ~ifelse(is.na(.) | . != max_ifc, max_ifc, .))) %>% 
#   select(-max_ifc)

## Run this code to make it the earliest year
ifc <- ifc %>% 
  mutate(across(matches("^fall|^spring"), ~ifelse(is.na(.) | . != earliest, earliest, .))) %>% 
  select(-earliest)


ifc_pivot <- ifc %>% 
  mutate(fall_ifc_2019 = ifelse(is.na(fall_ifc_2019), most_recent, fall_ifc_2019)) %>% 
  pivot_longer(cols = matches("\\d$"),
               names_to = "semester",
               values_to = "ifc_members") %>%
  extract(semester, "year", "(\\d{4})") %>% 
  select(-latitude, -longitude, -control_of_institution,
         -most_recent) 
ipeds <- ipeds %>% 
  filter(university %in% ifc::moratorium_schools()) %>% 
  select(university, year, undergraduate_enrollment, total_enrollment) 


frac_ifc <- ifc_pivot %>% 
  mutate(year = as.double(year)) %>% 
  left_join(ipeds) 


## this gives you the average fraction over the years of members to undergrads or ifc members to total enrollment
frac_ifc <- frac_ifc %>% 
  group_by(university) %>% 
  mutate(frac_ifc_undergrad = ifc_members/undergraduate_enrollment,
         frac_ifc_total = ifc_members/total_enrollment) %>% 
  ungroup() %>% 
  group_by(university) %>% 
  summarize(ifc_undergrad_frac = mean(frac_ifc_undergrad, na.rm = T),
            ifc_frac = mean(frac_ifc_total, na.rm = T))

## giving the deviations from the mean. Centering.
frac_ifc <- frac_ifc %>% 
  mutate(avg_ifc_undergrad_frac = mean(ifc_undergrad_frac),
         avg_ifc_frac  = mean(ifc_frac),
         deviation_undergrad_ifc = ifc_undergrad_frac - avg_ifc_undergrad_frac,
         deviation_ifc =  ifc_frac - avg_ifc_frac)


ifc_quartiles_undergrad <- quantile(frac_ifc$ifc_undergrad_frac) 
ifc_quartiles <- quantile(frac_ifc$ifc_frac) 
ifc_median <- median(frac_ifc$ifc_undergrad_frac)

create_quantiles_undergrad <- . %>% 
  mutate(ifc_quantile_1 = ifelse(ifc_undergrad_frac <= ifc_quartiles_undergrad[[2]], 1, 0),
         ifc_quantile_2 = ifelse(ifc_undergrad_frac <= ifc_quartiles_undergrad[[3]] & ifc_undergrad_frac > ifc_quartiles_undergrad[[2]], 1, 0),
         ifc_quantile_3 = ifelse(ifc_undergrad_frac <= ifc_quartiles_undergrad[[4]] & ifc_undergrad_frac > ifc_quartiles_undergrad[[3]], 1, 0),
         ifc_quantile_4 = ifelse(ifc_undergrad_frac > ifc_quartiles_undergrad[[4]], 1, 0),
         ifc_median_over = ifelse(ifc_undergrad_frac > ifc_median, 1, 0))

create_quantiles <- . %>% 
  mutate(ifc_quantile_1 = ifelse(ifc_frac <= ifc_quartiles[[2]], 1, 0),
         ifc_quantile_2 = ifelse(ifc_frac <= ifc_quartiles[[3]] & ifc_frac > ifc_quartiles[[2]], 1, 0),
         ifc_quantile_3 = ifelse(ifc_frac <= ifc_quartiles[[4]] & ifc_frac > ifc_quartiles[[3]], 1, 0),
         ifc_quantile_4 = ifelse(ifc_frac > ifc_quartiles[[4]], 1, 0))

daily_crime <- daily_crime %>% 
  left_join(frac_ifc) %>% 
  create_quantiles_undergrad() %>% 
  create_quantiles() %>% 
  mutate(treatment_ifc_dev_undergrad = treatment * deviation_undergrad_ifc,
         treatment_ifc_dev = treatment * deviation_ifc)


daily_crime_weekends <- daily_crime_weekends %>% 
  left_join(frac_ifc) %>% 
  create_quantiles_undergrad() %>% 
  create_quantiles() %>% 
  mutate(treatment_ifc_dev_undergrad = treatment * deviation_undergrad_ifc,
         treatment_ifc_dev = treatment * deviation_ifc)


daily_crime_weekdays <- daily_crime_weekdays %>% 
  left_join(frac_ifc) %>% 
  create_quantiles_undergrad() %>% 
  create_quantiles() %>% 
  mutate(treatment_ifc_dev_undergrad = treatment * deviation_undergrad_ifc,
         treatment_ifc_dev = treatment * deviation_ifc)


data_list <-  list(daily_crime, daily_crime_weekends,daily_crime_weekdays)


## the treatment variable gives you the effect on the outcome that results from an increase in the treatment intensity. 
explanatory_vars <-  c("treatment_ifc_dev_undergrad", "treatment")


fixed_effects <- c("day_of_week", "university_by_academic_year", "holiday", "spring_semester", "game_occurred")
alc_intensity <- map(data_list, ~ifc::reghdfe(.x , c("alcohol_offense_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))
sex_intensity <- map(data_list, ~ifc::reghdfe(.x, c("sexual_assault_per25"),explanatory_vars, fixed_effects = fixed_effects, "university"))

# table -------------------------------------------------------------------
gof_mapping <- ifc::gof_mapping() %>%
  select(-fmt) %>%
  mutate(fmt = ifelse(raw == "nobs", 0, 3)) %>%
  add_row(raw = "mean", clean = "Mean of Dependent Variable", fmt = 3, .after = 1)



footnote_ifc <- list("Fraction IFC is the average share of undergraduates that are in an IFC fraternity, centered at the mean. 
                     Note that not every university keeps record of their IFC numbers over time, and therefore,
                     the most recent number of IFC members is used in this calculation when sample-period data is missing. However, based on the few universities that provided year-to-year data on their IFC populations, the total number does not substantially change over time. 
                     Standard errors shown in parenthesis are clustered by university (37 clusters) and each offense is defined as per-25000 enrolled students.
                     The interaction of In Moratorium and Fraction IFC gives a measure of moratorium intensity based on the fraction of IFC members.
                     The regression specification is the preferred specification which includes day of week, holiday, football game-day, semester, and university-by-acacdemic-year fixed effects.",
                     "* p < 0.1, ** p < 0.05, *** p < 0.01") %>% 
  map(~str_replace_all(.x, "\n", ""))


ifc_share <- panelsummary::panelsummary(alc_intensity, sex_intensity,
                                        panel_labels = c("Panel A: Alcohol Offenses",
                                                         "Panel B: Sexual Assaults"),
                                        italic = T,
                                        bold = F,
                                        collapse_fe = T,
                                        mean_dependent = T,
                                        stars ="econ",
                                        gof_map = gof_mapping,
                                        coef_map = c("treatment" = "In Moratorium",
                                                     "treatment_ifc_dev_undergrad" = "In Moratorium x Fraction IFC"),
                                        caption = "\\label{ifc_share}The Effect of Moratoriums Interacted with IFC Share") %>% 
  add_header_above(c(" " = 1, "All Days" = 1, "Weekends" = 1, "Weekdays" = 1)) %>% 
  footnote(footnote_ifc,
           threeparttable = T)

data_list <-  list("All Days" = daily_crime, "Weekends" = daily_crime_weekends, "Weekdays" = daily_crime_weekdays)


alc_ifc <- map_df(data_list, ~feols(alcohol_offense_per25 ~
                                      treatment:ifc_quantile_1 + treatment:ifc_quantile_2 + treatment:ifc_quantile_3 +
                                      treatment:ifc_quantile_4| day_of_week + university_by_academic_year + holiday + spring_semester + game_occurred,
                                    cluster = ~university, data = .x ) %>% 
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
