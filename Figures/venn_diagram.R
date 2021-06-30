

library(VennDiagram)

appended_crime_logs <- read_csv("Created Data/xMaster_data_2021/appended_crime_logs.csv")

venn_data <- appended_crime_logs %>% 
  count(alcohol_offense, sexual_assault, drug_offense) 
alcohol_and_sex <- appended_crime_logs %>% 
  count(alcohol_offense, sexual_assault) %>% 
  filter(alcohol_offense == 1 & sexual_assault == 1) %>% 
  pull()
 
alcohol_and_drug <- appended_crime_logs %>% 
  count(alcohol_offense, drug_offense) %>% 
  filter(alcohol_offense == 1 & drug_offense == 1) %>% 
  pull()

drug_and_sex <- appended_crime_logs %>% 
  count(drug_offense, sexual_assault) %>% 
  filter(drug_offense == 1 & sexual_assault == 1) %>% 
  pull()

all_three <- appended_crime_logs %>% 
  count(alcohol_offense, sexual_assault, drug_offense) %>% 
  filter(alcohol_offense == 1 & sexual_assault == 1 & drug_offense == 1) %>% 
  pull()

alcohol <- appended_crime_logs %>% 
  count(alcohol_offense) %>% 
  filter(alcohol_offense == 1) %>% 
  pull()
drug <- appended_crime_logs %>% 
  count(drug_offense) %>% 
  filter(drug_offense == 1) %>% 
  pull()
sex <- appended_crime_logs %>% 
  count(sexual_assault) %>% 
  filter(sexual_assault == 1) %>% 
  pull()

sex_final <- sex - drug_and_sex - all_three - alcohol_and_sex
drug_final <- drug - drug_and_sex - all_three - alcohol_and_drug
alcohol_final <- alcohol - alcohol_and_sex - all_three - alcohol_and_drug

crime_diagram <- draw.triple.venn(
  area1 = sex, 
  area2 = drug,
  area3 = alcohol,
  n12 = drug_and_sex,
  n23 = alcohol_and_drug,
  n13 = alcohol_and_sex,
  n123 = all_three,
  category = c("Sexual Assault", "Drug Offense", "Alcohol Offense"),
  fill = c("blue", 'red', 'green'),
  lty = "blank",
  cex = 2,
  cat.cex = 2,
  cat.col = c('blue', 'red', 'green'),
  euler.d = T,
  print.mode = "percent"
)
