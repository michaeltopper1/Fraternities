
###################################################################################
##  This file creates the boxplot for the average per_capita rape by police department  ##
###################################################################################

load("/Users/michaeltopper/Desktop/Fraternities and Sexual Assault/Created Data/Master_data/master_ucr_ipeds_cross.rda")


## creating the outlier function:
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}


boxplot_department <- ucr_master %>% 
  mutate(subtype2 = ifelse(subtype2 == "(011) Four-year university", "University Police Department", "Local Municipality")) %>% 
  group_by(subtype2, name) %>% 
  summarize(avg_rape = mean(rape_per_hundredthousand, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(outlier = ifelse(is_outlier(avg_rape), as.character(name), as.character(NA))) %>% 
  ggplot(aes(y = avg_rape)) +
  geom_boxplot(color = "black", alpha = 0.8) +
  #geom_text(aes(x = "", label = outlier), na.rm = T, hjust = -0.1, size = 2.5) +
  facet_wrap(~subtype2) +
  labs(x = "", y = "", title = "") +
  theme_light() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) 

