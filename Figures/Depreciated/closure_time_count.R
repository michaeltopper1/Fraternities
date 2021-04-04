#######################################################################################
##  This file makes the bar graph of counts of closures over time by IFC/University  ##
#######################################################################################


load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Closure Dates/closure_dates.rda")

count_1 <- closure_table_round %>% 
  mutate(closure_year = year(closure)) %>% 
  group_by(university_enacted_1) %>%
  rename("university_enacted"= university_enacted_1) %>% 
  count(closure_year, name = "closure1_count")

count_2 <- closure_table_round %>% 
  mutate(closure_year = year(closure2)) %>% 
  group_by(university_enacted_2) %>% 
  rename("university_enacted" = university_enacted_2) %>% 
  count(closure_year, name = "closure2_count") %>% 
  filter(!is.na(closure_year))

closure_time_count <- count_1 %>% 
  left_join(y = count_2) %>% 
  mutate(closure2_count = ifelse(is.na(closure2_count), 0, closure2_count)) %>% 
  mutate(total_closures = closure1_count + closure2_count) %>% 
  mutate(university_enacted = as.factor(ifelse(university_enacted == 1, "University", "IFC"))) %>% 
  arrange(closure_year) %>% 
  ggplot(aes(x = as.factor(closure_year), y = total_closures)) +
  geom_col(aes(fill = university_enacted)) +
  labs(x = "", y = "", fill= "") +
  ggthemes::theme_calc()
