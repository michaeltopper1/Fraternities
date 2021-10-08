library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)
library(fwildclusterboot)
library(kableExtra)


# loading in data ---------------------------------------------------------

if (!exists("daily_crime")) {
  daily_crime <- read_csv(here::here("Created Data/xMaster_data_2021/daily_panel.csv")) %>% 
    filter(university %in% ifc::moratorium_schools()) %>% 
    group_by(university, year, semester_number) %>% 
    mutate(university_by_year_by_semester_number = cur_group_id()) %>% 
    ungroup() %>% 
    group_by(university, year) %>% 
    mutate(university_by_year = cur_group_id()) %>% 
    ungroup() %>% 
    group_by(university, year, month) %>% 
    mutate(university_by_month_by_year = cur_group_id()) %>% 
    ungroup() %>% 
    rename(day_of_week = weekday,
           university_by_semester_number = uni_semester)
}



if (!exists("daily_crime_weekdays")) {
  daily_crime_weekdays <- daily_crime %>% 
    filter(day_of_week == "Mon" | day_of_week == "Thu" | day_of_week == "Wed" | day_of_week == "Tue" )
}

if (!exists("daily_crime_weekends")) {
  daily_crime_weekends <- daily_crime %>%
    filter(day_of_week == 'Fri' | day_of_week == "Sat" | day_of_week == "Sun")
}
# regressions full sample-------------------------------------------------------------
alc_ols_1 <- daily_crime %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + year + university,
        vcov = cluster ~ university, data = .)

alc_ols_2 <- daily_crime %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .)

alc_ols_3 <- daily_crime %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
alc_ols_4 <- daily_crime %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .)
alc_ols <- list("(1)" = alc_ols_1, "(2)" = alc_ols_2, "(3)" = alc_ols_3, "(4)" = alc_ols_4)


# regressions weekends ----------------------------------------------------


alc_weekend_1 <- daily_crime_weekends %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + year + university,
        cluster = ~university, data = .) 
alc_weekend_2 <- daily_crime_weekends %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .) 
alc_weekend_3 <- daily_crime_weekends %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
alc_weekend_4 <- daily_crime_weekends %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .) 

# regressions weekdays ----------------------------------------------------


alc_weekdays_1 <- daily_crime_weekdays %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + year + university,
        cluster = ~university, data = .) 
alc_weekdays_2 <- daily_crime_weekdays %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .) 
alc_weekdays_3 <- daily_crime_weekdays %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
alc_weekdays_4 <- daily_crime_weekdays %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .) 


# regressions full sample-------------------------------------------------------------
sex_ols_1 <- daily_crime %>% 
  feols(sexual_assault_per25 ~ treatment | day_of_week + year + university,
        vcov = cluster ~ university, data = .)

sex_ols_2 <- daily_crime %>% 
  feols(sexual_assault_per25 ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .)

sex_ols_3 <- daily_crime %>% 
  feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
sex_ols_4 <- daily_crime %>% 
  feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .)
sex_ols <- list("(1)" = sex_ols_1, "(2)" = sex_ols_2, "(3)" = sex_ols_3, "(4)" = sex_ols_4)


# regressions weekends ----------------------------------------------------


sex_weekend_1 <- daily_crime_weekends %>% 
  feols(sexual_assault_per25 ~ treatment | day_of_week + year + university,
        cluster = ~university, data = .) 
sex_weekend_2 <- daily_crime_weekends %>% 
  feols(sexual_assault_per25 ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .) 
sex_weekend_3 <- daily_crime_weekends %>% 
  feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
sex_weekend_4 <- daily_crime_weekends %>% 
  feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .) 

# regressions weekdays ----------------------------------------------------


sex_weekdays_1 <- daily_crime_weekdays %>% 
  feols(sexual_assault_per25 ~ treatment | day_of_week + year + university,
        cluster = ~university, data = .) 
sex_weekdays_2 <- daily_crime_weekdays %>% 
  feols(sexual_assault_per25 ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .) 
sex_weekdays_3 <- daily_crime_weekdays %>% 
  feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
sex_weekdays_4 <- daily_crime_weekdays %>% 
  feols(sexual_assault_per25 ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .) 



alcohol_sex_ols <- list("(1)" = alc_ols_1, "(2)" = alc_ols_2, "(3)" = alc_ols_3, "(4)" = alc_ols_4,
                        "(1)" = sex_ols_1, "(2)" = sex_ols_2, "(3)" = sex_ols_3, "(4)" = sex_ols_4)


# functions for cleaning table --------------------------------------------

## This function will take a fixest object and put it into a tidy data frame
## In particular, it takes the pvalue, nobs, estimate, and standard error
## and adds in stars and paranethesis to the point estimates and standard errors respectively

get_est <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {
  coeftable <- summary(x, ...)$coeftable
  ret <- as_tibble(coeftable, rownames = "term")
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
  if (conf.int) {
    CI <- stats::confint(x, level = conf.level, ...)
    # Bind to rest of tibble
    colnames(CI) <- c("conf.low", "conf.high")
    ret <- bind_cols(ret, unrowname(CI))
  }
  table <- as_tibble(ret)
  obs <- x %>% nobs()
  obs_tibble <- tribble(~Num.Obs.,
                        obs)
  table <- bind_cols(table, obs_tibble)
  pivoted <- table %>% 
    select(estimate, std.error, p.value, Num.Obs.) %>% 
    pivot_longer(cols = everything(), values_to = "est", names_to = "stat")
  pvalue = pivoted[[3,2]]
  pivoted <- pivoted %>% 
    mutate(est = as.character(sprintf(est,fmt = "%.3f")))
  if (pvalue < 0.1 & pvalue > 0.05) {
    pivoted[[1,2]] <- paste0(pivoted[[1,2]], "+")
  }
  if (pvalue < 0.05 & pvalue >= 0.01){
    pivoted[[1,2]] <- paste0(pivoted[[1,2]], "*")
  }
  if (pvalue < 0.01 & pvalue >= 0.001){
    pivoted[[1,2]] <- paste0(pivoted[[1,2]], "**")
  }
  if (pvalue < 0.001){
    pivoted[[1,2]] <- paste0(pivoted[[1,2]], "***")
  }
  pivoted[[2,2]] <- paste0("(", pivoted[[2,2]], ")")
  pivoted[[4,2]] <- str_replace(pivoted[[4,2]], "\\..{1,}", "")
  return(pivoted)
}

## Reduces the data frame from get_est
reduce_frame <- function(x){
  new <- x %>% 
    rename("term" = "stat...1") %>% 
    select(-matches("^stat"))
  return(new)
}

## This function will give a tibble of means for the dependent variable of interst
## The input is a dataframe. it outputs a tibble with the same columnnames as the connected
## functions above
get_means <- function(x, column){
  mean <- x %>% 
    summarize(mean({{column}}, na.rm = T)) %>% 
    pull()
  mean_vector <- rep(mean, 5)
  mean_tibble <- tribble(~term, ~`est...2`, ~`est...3`, ~`est...4`, ~`est...5`,
                         "Mean of Dependent Variable", mean, mean, mean, mean)
  mean_tibble <- mean_tibble %>% 
    mutate(across(c(2:5), ~as.character(sprintf(.,fmt = "%.3f"))))
  return(mean_tibble)
}



# creating table ----------------------------------------------------------

## getting the dependent variable means
initial_means_alc <- get_means(daily_crime, alcohol_offense_per25)
initial_means_sex <- get_means(daily_crime, sexual_assault_per25)

initial_means <- bind_cols(initial_means_alc, initial_means_sex) %>% 
  select(-`term...6`) %>% 
  rename("term" = "term...1")

## appending all the weekend columns together
models <- list(alc_weekend_1, alc_weekend_2, alc_weekend_3, alc_weekend_4,
               sex_weekend_1, sex_weekend_2, sex_weekend_3, sex_weekend_4)

## performing the get_est function on all the models, then combining them together and gettingn rid of the pvale
## I then append the means
weekend_means_alc <- get_means(daily_crime_weekends, alcohol_offense_per25)
weekend_means_sex <- get_means(daily_crime_weekends, sexual_assault_per25)
weekend_means <- bind_cols(weekend_means_alc, weekend_means_sex) %>% 
  select(-`term...6`) %>% 
  rename("term" = "term...1")

add_weekends <- map(models, ~get_est(.x)) %>% 
  reduce(bind_cols) %>% 
  reduce_frame() %>% 
  slice(-c(3))  %>% 
  bind_rows(weekend_means)


## This is to add the means of the estimated model for the full sample first
add_weekends <- initial_means %>% 
  bind_rows(add_weekends)


## Now i do a similar thing with the weekday models
models_weekdays <- list(alc_weekdays_1, alc_weekdays_2, alc_weekdays_3, alc_weekdays_4,
                        sex_weekdays_1, sex_weekdays_2, sex_weekdays_3, sex_weekdays_4)

## similar to above
weekday_means_alc <- get_means(daily_crime_weekdays, alcohol_offense_per25)
weekday_means_sex <- get_means(daily_crime_weekdays, sexual_assault_per25)
weekend_means <- bind_cols(weekday_means_alc, weekday_means_sex) %>% 
  select(-`term...6`) %>% 
  rename("term" = "term...1")

add_weekdays <- map(models_weekdays, ~get_est(.x)) %>% 
  reduce(bind_cols) %>% 
  reduce_frame() %>% 
  slice(-c(3)) %>% 
  bind_rows(weekend_means)

## creating the final tibble to be added to the table
add_rows <- add_weekends %>% 
  bind_rows(add_weekdays)

## this line will change certain row terms
add_rows <- add_rows %>% 
  mutate(term = replace(term, term == "estimate", "Moratorium"),
         term = replace(term, term == "std.error", " "))

## adjusting where the position should be in the table
attr(add_rows, 'position') <- c(4:12)

## This maps the fixed effects clean up
gm <- tribble(~raw, ~clean, ~fmt,
              "Std.Errors", "Cluster", ~fmt,
              "FE: day_of_week","FE: Day-of-Week", ~fmt,
              "FE: semester_number", "FE: Semester-Number", ~fmt,
              "FE: university","FE: University", ~fmt,
              "FE: university_by_year_by_semester_number", "FE: University-by-Year-by-Semester-Number", ~fmt)

## Final table
main_regressions <- modelsummary(alcohol_sex_ols,
                             stars = T, 
                             gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
                             coef_map = c("treatment" = "Moratorium"),
                             title = "\\label{main_ols}Effect of Moratoriums on Alcohol Offenses and Sexual Assault (OLS).",
                             notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                                          "Standard errors are clustered by university.",
                                          "Outcomes of interest are alcohol offenses and reports of sexual assaults per 25 thousand students.",
                                          "Coefficient estimates shown are for Moratorium."),
                             add_rows = add_rows) %>% 
  pack_rows("Full Sample (Monday - Sunday)",1,4) %>% 
  pack_rows("Weekends (Friday - Sunday)", 5, 8) %>% 
  pack_rows("Weekdays (Monday - Thursday)",9, 12) %>% 
  add_header_above(c(" " = 1, "Alcohol Offense" = 4, "Sexual Assault" = 4))





