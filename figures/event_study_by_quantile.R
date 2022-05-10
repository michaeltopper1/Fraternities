

source(here::here(paste0("figures/event_study_final_dec13.R")))



moratorium_ids <- daily_crime %>% 
  group_by(university) %>% 
  mutate(treatment_na = ifelse(treatment == 0, NA, treatment)) %>% 
  mutate(treatment_na = ifelse(treatment_na > 0 & (!is.na(closure_2)) & date >= closure_2, 2, treatment_na)) %>% 
  mutate(treatment_na = ifelse(treatment_na > 1 & (!is.na(closure_3)) & date >= closure_3, 3, treatment_na)) %>% 
  mutate(treatment_na = ifelse(is.na(treatment_na), 0, treatment_na)) %>% 
  ungroup() %>% 
  filter(treatment_na >0) %>% 
  group_by(treatment_na, university) %>% 
  mutate(moratorium_id = cur_group_id()) %>% 
  ungroup() %>% 
  select(moratorium_id, university, treatment,date)

daily_crime <- daily_crime %>% 
  left_join(moratorium_ids) %>% 
  mutate(moratorium_id = ifelse(is.na(moratorium_id), 0, moratorium_id))


moratorium_lengths <- daily_crime %>% 
  group_by(moratorium_id) %>% 
  mutate(length_moratorium = sum(treatment)) %>% 
  select(moratorium_id, length_moratorium, university) %>% 
  ungroup() %>% 
  filter(moratorium_id != 0) %>% 
  distinct(length_moratorium, university, moratorium_id) 

quartiles <- quantile(moratorium_lengths$length_moratorium, c(0.33, .66, 1))

daily_crime <- daily_crime %>% 
  left_join(moratorium_lengths) %>% 
  mutate(length_moratorium = ifelse(is.na(length_moratorium), 0, length_moratorium)) %>% 
  mutate(below_q33 = if_else(between(length_moratorium, 0.1,quartiles[[1]]), 1,0)) %>% 
  mutate(between_q33_q66 = ifelse(between(length_moratorium, quartiles[[1]], quartiles[[2]]), 1, 0)) %>% 
  mutate(above_q66 = ifelse(between(length_moratorium, quartiles[[2]], quartiles[[3]]), 1, 0)) 





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



es_sex_14_lower <- ifc::reghdfe(es_14 %>% 
                                  filter(university %in% lower_quantiles), "sexual_assault_per25", explanatory_vars_14, fixed_effects, cluster = "university")
es_sex_14_middle <- ifc::reghdfe(es_14 %>% 
                                   filter(university %in% middle_quantiles), "sexual_assault_per25", explanatory_vars_14, fixed_effects, cluster = "university")
es_sex_14_last <- ifc::reghdfe(es_14 %>% 
                                 filter(university %in% last_quantiles), "sexual_assault_per25", explanatory_vars_14, fixed_effects, cluster = "university")

ifc::event_study_graph(es_sex_14_lower, 5)
ifc::event_study_graph(es_sex_14_middle, 5)
ifc::event_study_graph(es_sex_14_last, 5)


alc_es <- list(es_alc_14, es_alc_14_lower, es_alc_14_middle, es_alc_14_last)

sex_es <- list(es_sex_14 , es_sex_14_lower, es_sex_14_middle, es_sex_14_last)


alc_lag_f <- map_df(alc_es, ~car::linearHypothesis(.,
                              c("beta_lag_1 = 0",
                                "beta_lag_2 =0",
                                "beta_lag_3 =0",
                                "beta_lag_4 =0")) %>% 
      broom::tidy()) %>% 
  filter(if_all(everything(), ~!is.na(.)))

sex_lag_f <- map_df(sex_es, ~car::linearHypothesis(.,
                                      c("beta_lag_1 = 0",
                                        "beta_lag_2 =0",
                                        "beta_lag_3 =0",
                                        "beta_lag_4 =0")) %>% 
         broom::tidy()) %>% 
  filter(if_all(everything(), ~!is.na(.)))



# table -------------------------------------------------------------------


long_run_effects <- ifc::main_table(list(es_alc_14,es_sex_14),
                                    list(es_alc_14_lower, es_sex_14_lower),
                                    list(es_alc_14_middle, es_sex_14_middle), 
                                    last_panel =list(es_alc_14_last, es_sex_14_last)) %>% 
  slice(1:12) %>% 
  add_row(term = "F-test P-value of Lags", 
          `Model 1` = sprintf("%.3f",alc_lag_f$p.value[[1]]),
          `Model 2` = sprintf("%.3f",sex_lag_f$p.value[[1]]),
          .before = 4) %>% 
  add_row(term = "F-test P-value of Lags", 
          `Model 1` = sprintf("%.3f",alc_lag_f$p.value[[2]]),
          `Model 2` = sprintf("%.3f",sex_lag_f$p.value[[2]]),
          .before = 8) %>% 
  add_row(term = "F-test P-value of Lags", 
          `Model 1` = sprintf("%.3f",alc_lag_f$p.value[[3]]),
          `Model 2` = sprintf("%.3f",sex_lag_f$p.value[[3]]),
          .before = 12) %>% 
  add_row(term = "F-test P-value of Lags", 
          `Model 1` = sprintf("%.3f",alc_lag_f$p.value[[4]]),
          `Model 2` = sprintf("%.3f",sex_lag_f$p.value[[4]]),
          .before = 16) %>% 
  kbl(col.names = c(" ", "Alcohol Offenses", "Sexual Assaults"),
      booktabs = T,
      caption = "\\label{long_run_effects}Absence of Dynamic Effects of Moratoriums Split by Moratorium Length") %>% 
  kable_styling() %>% 
  group_rows("Panel A: Full Sample", 1,4, bold = F, italic = T) %>% 
  group_rows("Panel B: Quantiles by Moratorium Length", 5,16, bold = F, italic = T) %>% 
  pack_rows("Estimates from Figures 4 and 5", 1, 4, bold = F, italic = T) %>% 
  pack_rows("Moratorium Length: 1st Quantile", 5, 8, bold = F, italic = T, latex_gap_space = "0.5cm") %>% 
  pack_rows("Moratorium Length: 2nd Quantile", 9, 12, bold = F, italic = T, latex_gap_space = "0.5cm") %>% 
  pack_rows("Moratorium Length: 3rd Quantile", 13, 16, bold = F, italic = T, latex_gap_space = "0.5cm") %>% 
  add_header_above(c(" ", "Dependent Variable" = 2)) %>% 
  column_spec(1, width = "8cm") %>% 
  row_spec(0, hline_after = T) %>% 
  row_spec(4, hline_after = T) %>% 
  row_spec(4, hline_after = T) %>% 
  footnote(list("Point estimates of In Motratorium reflect the time 0 for `multiple event' event studies similar to Figures 4 and 5 with four leads and four lags of 14-day bins. Each offense is defined as per-25,000 enrolled students. Standard errors are clustered at the university level. All periods are normalized by the 14-day period before the moratorium. Panel A represents the same coefficient estimates as Figures 4 and 5, while Panels B,C, and D represent subsets of the sample split by three quantiles. The three quantiles represent the 33rd, 66th, and 100th percentile of a moratorium length which correspond to [0-32], [33-59], and [60-541] academic calendar days of a moratorium respectively. Hence, if a university has a moratorium that lasts 30 academic calendar days, then it is included in Panel B. P-values are reported from joint F-test of the four lags. Fixed effects include day of the week, holiday, semester number, football game-day, and university-by-academic-year.",
                "* p < 0.1, ** p < 0.05, *** p < 0.01"), threeparttable = T)