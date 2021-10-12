## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-08
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

if (!exists("nibrs_all")) {
  nibrs_schools <- read_csv("Created Data/nibrs/final_panel_all.csv")
}

if (!exists("nibrs_schools")) {
  nibrs_schools <- read_csv("Created Data/nibrs/final_panel_schools.csv")
}

if (!exists("nibrs_nonschools")){
  nibrs_nonschools <- read_csv("Created Data/nibrs/final_panel_nonschools.csv")
}


## function that returns the mean 
get_means_sex <- function(df) {
  mean_sex <- df %>% 
    summarize(mean(college_age_sexual_assault, na.rm = T)) %>% 
    pull()
  return(mean_sex)
}


nibrs <- list(nibrs_schools, nibrs_schools, nibrs_nonschools)
nibrs <- purrr::map(nibrs, ~.x %>% 
      mutate(day_of_week = lubridate::wday(date, label = T), year = lubridate::year(date)) %>% 
      group_by(university, semester_number) %>% 
      mutate(university_by_semester_number = cur_group_id()) %>% 
      ungroup() %>% 
      group_by(university, year, semester_number) %>% 
      mutate(university_by_year_by_semester_number = cur_group_id()) %>% 
      ungroup() %>% 
      mutate(college_age_sexual_assault = college_age_rape + college_age_fondle + college_age_rape_statutory +
               college_age_sexual_assault_object)
      )
nibrs_schools <- nibrs[[1]]
nibrs_schools <- nibrs[[2]]
nibrs_nonschools <- nibrs[[3]]  

nibrs_all_weekdays <- nibrs_schools %>%
  filter(day_of_week == "Mon" | day_of_week == "Thu" | day_of_week == "Wed" | day_of_week == "Tue" )
nibrs_schools_weekends <- nibrs_schools %>% 
  filter(day_of_week == 'Fri' | day_of_week == "Sat" | day_of_week == "Sun")

nibrs_schools_weekends <- nibrs_schools %>% 
  filter(day_of_week == 'Fri' | day_of_week == "Sat" | day_of_week == "Sun")
nibrs_schools_weekdays <- nibrs_schools %>% 
  filter(day_of_week == "Mon" | day_of_week == "Thu" | day_of_week == "Wed" | day_of_week == "Tue" )


nibrs_nonschools_weekdays <- nibrs_nonschools %>% 
  filter(day_of_week == "Mon" | day_of_week == "Thu" | day_of_week == "Wed" | day_of_week == "Tue" )
nibrs_nonschools_weekends <- nibrs_nonschools %>% 
  filter(day_of_week == 'Fri' | day_of_week == "Sat" | day_of_week == "Sun")


# nibrs_all regressions - all ori9s including lindo away ------------------
all_1 <- nibrs_schools %>% 
  feols(college_age_sexual_assault~ treatment | day_of_week + year + university,
        vcov = cluster ~ university, data = .)
all_2 <- nibrs_schools %>% 
  feols(college_age_sexual_assault ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .)
all_3 <- nibrs_schools %>% 
  feols(college_age_sexual_assault ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
all_4 <- nibrs_schools %>% 
  feols(college_age_sexual_assault ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .)

all_1_weekend <- nibrs_schools_weekends %>% 
  feols(college_age_sexual_assault~ treatment | day_of_week + year + university,
        vcov = cluster ~ university, data = .)
all_2_weekend <- nibrs_schools_weekends %>% 
  feols(college_age_sexual_assault ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .)
all_3_weekend <- nibrs_schools_weekends %>% 
  feols(college_age_sexual_assault ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
all_4_weekend <- nibrs_schools_weekends %>% 
  feols(college_age_sexual_assault ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .)

all_models <- list("(1)" = all_1, "(2)" = all_2, "(3)" = all_3, "(4)" = all_4,
                   "(1)" = all_1_weekend, "(2)" = all_2_weekend, "(3)" = all_3_weekend, "(4)" = all_4_weekend)

# creating dependent variable row means -----------------------------------



all_models_row_add <- tribble(~term, ~total1, ~total2, ~total3, ~total4, ~week1, ~week2, ~week3, ~week4,
                              "Mean of Dependent Variable", get_means_sex(nibrs_schools), get_means_sex(nibrs_schools), 
                              get_means_sex(nibrs_schools), get_means_sex(nibrs_schools), 
                              get_means_sex(nibrs_schools_weekends), get_means_sex(nibrs_schools_weekends),
                              get_means_sex(nibrs_schools_weekends),get_means_sex(nibrs_schools_weekends))

attr(all_models_row_add, "position") <- 4


# creating modelsummary for th info ---------------------------------------

nibrs_all_table <- modelsummary(all_models,
             stars = T, 
             gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
             coef_map = c("treatment" = "Moratorium"),
             add_rows = all_models_row_add,
             title = "\\label{nibrs_all}Effect of Moratoriums on College-Aged Sexual Assault Victims includes surrounding police agencies.",
             notes = list("Dependent variable is college-aged sexual assaults which is combination of fondling, rape, and sexual assault w object.",
                          "Standard errors clustered by university.",
                          "Only 14 of 38 universities included in sample due to reporting issues.",
                          "College aged includes ages 17 to 22.")) %>% 
  add_header_above(c(" " = 1, "Total Sample" = 4, "Weekends Only" = 4)) %>% 
  add_header_above(c(" " = 1, "NIBRS Data University Police and Surrounding Area" = 8))

# nibrs_schools regressions -----------------------------------------------
schools_1 <- nibrs_schools %>% 
  feols(college_age_sexual_assault~ treatment | day_of_week + year + university,
        vcov = cluster ~ university, data = .)
schools_2 <- nibrs_schools %>% 
  feols(college_age_sexual_assault ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .)
schools_3 <- nibrs_schools %>% 
  feols(college_age_sexual_assault ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
schools_4 <- nibrs_schools %>% 
  feols(college_age_sexual_assault ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .)



schools_1_weekend <- nibrs_schools_weekends %>% 
  feols(college_age_sexual_assault~ treatment | day_of_week + year + university,
        vcov = cluster ~ university, data = .)
schools_2_weekend <- nibrs_schools_weekends %>% 
  feols(college_age_sexual_assault ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .)
schools_3_weekend <- nibrs_schools_weekends %>% 
  feols(college_age_sexual_assault ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
schools_4_weekend <- nibrs_schools_weekends %>% 
  feols(college_age_sexual_assault ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .)

schools_models <- list("(1)" = schools_1, "(2)" = schools_2, "(3)" = schools_3, "(4)" = schools_4,
                   "(1)" = schools_1_weekend, "(2)" = schools_2_weekend, "(3)" = schools_3_weekend, "(4)" = schools_4_weekend)



# creating dependent variable means to add --------------------------------


schools_models_row_add <- tribble(~term, ~total1, ~total2, ~total3, ~total4, ~week1, ~week2, ~week3, ~week4,
                              "Mean of Dependent Variable", get_means_sex(nibrs_schools), get_means_sex(nibrs_schools), 
                              get_means_sex(nibrs_schools), get_means_sex(nibrs_schools), 
                              get_means_sex(nibrs_schools_weekends), get_means_sex(nibrs_schools_weekends),
                              get_means_sex(nibrs_schools_weekends),get_means_sex(nibrs_schools_weekends))

attr(schools_models_row_add, 'position') <- c(4)



# final model summary for schools -----------------------------------------


nibrs_schools_table <- modelsummary(schools_models,
             stars = T, 
             gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
             coef_map = c("treatment" = "Moratorium"),
             add_rows = schools_models_row_add,
             title = "\\label{nibrs_schools}Effect of Moratoriums on College-Aged Sexual Assault for Universities Reporting to NIBRS",
             notes = list("Dependent variable is college-aged sexual assaults which is combination of fondling, rape, and sexual assault object.",
                          "Standard errors clustered by university.",
                          "Only 14 of 38 universities included in sample due to reporting issues.",
                          "College aged includes ages 17 to 22.")) %>% 
  add_header_above(c(" " = 1, "Total Sample" = 4, "Weekends Only" = 4)) %>% 
  add_header_above(c(" " = 1, "NIBRS Data (University Police Only)" = 8))



