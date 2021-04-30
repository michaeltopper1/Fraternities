
library(tidyverse)
library(lubridate)
library(fixest)
library(ifc)

daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv",
                        guess_max = 50000)
daily_crime <- daily_crime %>% 
  mutate(across(starts_with("closure"), ~as_date(.)))

no_closure_uni <- daily_crime %>% 
  filter(is.na(closure_1)) %>% 
  distinct(university) %>% 
  pull

pseudo_crime <- trends_test(daily_crime, 8, 7) %>% 
  group_by(closure_1) %>% 
  mutate(closure_id = cur_group_id()) %>% 
  ungroup() 

closure_times_id <- pseudo_crime %>% 
  distinct(closure_id, closure_1, closure_1_end) %>% 
  rename("pseudo_closure_1" = closure_1,
         "pseudo_closure_1_end" = closure_1_end) 

closure_id_vec <- pseudo_crime %>% 
  distinct(closure_id) %>% 
  filter(closure_id != 45) %>% 
  pull()

set.seed(1992)

pseudo_crime <- pseudo_crime %>% 
  group_by(university) %>% 
  mutate(random_treatment = sample(closure_id_vec, 1)) %>% 
  ungroup() %>% 
  left_join(closure_times_id, by = c("random_treatment" = "closure_id")) %>% 
  mutate(closure_1 = ifelse(university %in% no_closure_uni, pseudo_closure_1, closure_1),
         closure_1_end = ifelse(university %in% no_closure_uni, pseudo_closure_1_end, closure_1_end)) %>% 
  mutate(across(c(closure_1, closure_1_end), ~ as_date(.))) %>% 
  mutate(treatment = ifelse(date >= closure_1 & date < closure_1_end, 1, 0))

pseudo_crime <- pseudo_crime %>% 
  select(-starts_with("closure_minus"), -starts_with("closure_plus"), -starts_with("treatment_plus"), -starts_with("treatment_minus"))

pseudo_crime <- pseudo_crime %>% 
  mutate(closure_2 = NA, closure_2_end = NA)

window <- c(1:8)
cluster_blocks <- 7
for (i in window) {
  closure_plus <- paste("closure_plus_", i, sep = "")
  closure_plus_end <- paste0("closure_plus_",i, "_end")
  colname_treat <- paste0("treatment_plus_", i)
  pseudo_crime <- pseudo_crime %>%
    mutate(!!sym(closure_plus) := (closure_1_end + (i-1) *days(cluster_blocks))) %>%
    mutate(!!sym(closure_plus_end) := closure_1_end + ((i) * days(cluster_blocks))) %>%
    mutate(!!sym(colname_treat) := ifelse(date >= !!sym(closure_plus) & date < !!sym(closure_plus_end), 1, 0))
}

for (i in window) {
  closure_minus <- paste("closure_minus_", i, sep = "")
  closure_minus_end <- paste0("closure_minus_",i, "_end")
  colname_treat <- paste0("treatment_minus_", i)
  pseudo_crime <- pseudo_crime %>%
    mutate(!!sym(closure_minus) := closure_1 -  i *days(cluster_blocks)) %>%
    mutate(!!sym(closure_minus_end) := closure_1 - ((i- 1) * days(cluster_blocks))) %>%
    mutate(!!sym(colname_treat) := ifelse(date >= !!sym(closure_minus) & date < !!sym(closure_minus_end), 1, 0))
}

summary <- pseudo_crime %>% 
  mutate(pseudo_uni = ifelse(university %in% no_closure_uni, 1, 0)) %>% 
  group_by(across(starts_with("treatment")), pseudo_uni) %>% 
  summarize(mean_alc = mean(alcohol_offense, na.rm = T)) %>% 
  ungroup()
summary %>% 
  filter(pseudo_uni == 1) %>% 
  select(-pseudo_uni) %>% 
  pivot_wider(mean_alc, names_from = c(-mean_alc), values_from = mean_alc) %>% View()

summary %>% 
  filter(pseudo_uni == 1) %>%
  
  