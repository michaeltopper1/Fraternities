## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2022-03-01
##

library(tidyverse)
library(kableExtra)
library(tidytext)
library(patchwork)
library(grid)
library(gridExtra)

if (!exists("daily_crime")) {
  daily_crime <- read_csv("created_data/xmaster_data/daily_panel.csv")
}


# getting averages  -------------------------------------------------------
averages <- daily_crime %>% 
  mutate(weekend = ifelse(day_of_week == "Fri" | day_of_week == "Sat" | day_of_week == "Sun", "Weekends", "Weekdays")) %>% 
  mutate(weekend = factor(weekend, levels = c("Weekends", "Weekdays"))) %>% 
  mutate(treatment = ifelse(treatment == 1, "moratorium", "not")) %>% 
  group_by(university, treatment, weekend) %>% 
  summarize(avg_alc = mean(alcohol_offense_per25, na.rm = T),
            avg_sex = mean(sexual_assault_per25, na.rm = T)) %>% 
  pivot_wider( names_from = treatment, values_from = c(avg_alc, avg_sex)) %>% 
  rowwise() %>% 
  mutate(alc_difference = avg_alc_moratorium -avg_alc_not,
         sex_difference = avg_sex_moratorium - avg_sex_not) %>% 
  ungroup() %>%
  group_by(weekend) %>% 
  mutate(avg_avg_difference_alc = mean(alc_difference),
         avg_avg_difference_sex = mean(sex_difference)) %>% 
  ungroup() 
alc_weekend <- averages %>% 
  filter(weekend == "Weekends") %>% 
  summarize(avg_alc_difference = mean(alc_difference)) %>% pull()

alc_weekday <- averages %>% 
  filter(weekend != "Weekends") %>% 
  summarize(avg_alc_difference = mean(alc_difference)) %>% pull()

sex_weekend <- averages %>% 
  filter(weekend == "Weekends") %>% 
  summarize(avg_sex_difference = mean(sex_difference)) %>% pull()

sex_weekday <- averages %>% 
  filter(weekend != "Weekends") %>% 
  summarize(avg_sex_difference = mean(sex_difference)) %>% pull()


alc_bar <- averages %>% 
  mutate(positive = ifelse(alc_difference >=0, "Average Offenses Higher in Moratorium", "Average Offenses Lower in Moratorium")) %>% 
  mutate(university = reorder_within(university, alc_difference, weekend)) %>% 
  ggplot(aes(university, alc_difference, fill = positive)) +
  geom_col() +
  geom_hline(aes(yintercept = avg_avg_difference_alc), linetype = "dashed") +
  scale_y_continuous(breaks = round(c(-2,-1,alc_weekend,0,1,1), 2)) +
  facet_wrap(~weekend, scales = "free_x") +
  scale_x_reordered() +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "",y = "", fill = " ", title = "Panel A: Alcohol Offenses") +
  theme_minimal() +
  theme(legend.position = "none", axis.text.x = element_blank(),
        strip.text = element_text(size = 12)) +
  ggsci::scale_fill_npg()


sex_bar <- averages %>% 
  mutate(university = reorder_within(university, sex_difference, weekend)) %>%
  mutate(positive = ifelse(sex_difference >=0, "Average Offenses Higher in Moratorium", "Average Offenses Lower in Moratorium")) %>% 
  ggplot(aes(university, sex_difference, fill = positive)) +
  geom_col() +
  geom_hline(aes(yintercept = avg_avg_difference_sex), linetype = "dashed") +
  scale_y_continuous(breaks = round(c(-0.1,sex_weekend,.1,.2), 2)) +
  facet_wrap(~weekend, scales = "free_x") +
  scale_x_reordered() +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "",y = " ", fill = " ", title = "Panel B: Sexual Assaults") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_blank(),
        strip.text = element_text(size = 12)) +
  ggsci::scale_fill_npg()

result <- alc_bar + sex_bar + plot_layout(nrow = 2) 

diff_average_offenses <- patchwork::patchworkGrob(result) 


