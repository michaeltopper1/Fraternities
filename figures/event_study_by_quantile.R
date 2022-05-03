
lower_quantiles <- daily_crime %>% 
  filter(below_q33 == 1) %>% 
  distinct(university) %>% 
  pull()

middle_quantiles <- daily_crime %>% 
  filter(between_q33_q66 == 1) %>% 
  distinct(university) %>% 
  pull()

last_quantiles <- daily_crime %>% 
  filter(above_q66 == 1) %>% 
  distinct(university) %>% 
  pull()

es_alc_14_lower <- ifc::reghdfe(es_14 %>% 
                                  filter(university %in% lower_quantiles), "alcohol_offense_per25", explanatory_vars_14, fixed_effects, cluster = "university")
es_alc_14_middle <- ifc::reghdfe(es_14 %>% 
                                   filter(university %in% middle_quantiles), "alcohol_offense_per25", explanatory_vars_14, fixed_effects, cluster = "university")
es_alc_14_last <- ifc::reghdfe(es_14 %>% 
                                 filter(university %in% last_quantiles), "alcohol_offense_per25", explanatory_vars_14, fixed_effects, cluster = "university")


ifc::event_study_graph(es_alc_14_lower, 5)
ifc::event_study_graph(es_alc_14_middle, 5)
ifc::event_study_graph(es_alc_14_last, 5)
