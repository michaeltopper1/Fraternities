library(tidyverse)
library(lubridate)
library(fixest)
library(modelsummary)
library(fwildclusterboot)
library(kableExtra)


# loading in data ---------------------------------------------------------

if (!exists("daily_crime")) {
  daily_crime <- read_csv(here::here("Created Data/xMaster_data_2021/daily_panel.csv")) 
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
alc_pois_1 <- daily_crime %>% 
  fepois(alcohol_offense ~ treatment | day_of_week + year + university,
        vcov = cluster ~ university, data = .)

alc_pois_2 <- daily_crime %>% 
  fepois(alcohol_offense ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .)

alc_pois_3 <- daily_crime %>% 
  fepois(alcohol_offense ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
alc_pois_4 <- daily_crime %>% 
  fepois(alcohol_offense ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .)



# regressions weekends ----------------------------------------------------


alc_p_weekend_1 <- daily_crime_weekends %>% 
  fepois(alcohol_offense ~ treatment | day_of_week + year + university,
        cluster = ~university, data = .) 
alc_p_weekend_2 <- daily_crime_weekends %>% 
  fepois(alcohol_offense ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .) 
alc_p_weekend_3 <- daily_crime_weekends %>% 
  fepois(alcohol_offense ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
alc_p_weekend_4 <- daily_crime_weekends %>% 
  fepois(alcohol_offense ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .) 

# regressions weekdays ----------------------------------------------------


alc_p_weekdays_1 <- daily_crime_weekdays %>% 
  fepois(alcohol_offense ~ treatment | day_of_week + year + university,
        cluster = ~university, data = .) 
alc_p_weekdays_2 <- daily_crime_weekdays %>% 
  fepois(alcohol_offense ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .) 
alc_p_weekdays_3 <- daily_crime_weekdays %>% 
  fepois(alcohol_offense ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
alc_p_weekdays_4 <- daily_crime_weekdays %>% 
  fepois(alcohol_offense ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .) 


# regressions full sample-------------------------------------------------------------
sex_pois_1 <- daily_crime %>% 
  fepois(sexual_assault ~ treatment | day_of_week + year + university,
        vcov = cluster ~ university, data = .)

sex_pois_2 <- daily_crime %>% 
  fepois(sexual_assault ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .)

sex_pois_3 <- daily_crime %>% 
  fepois(sexual_assault ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
sex_pois_4 <- daily_crime %>% 
  fepois(sexual_assault ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .)


# regressions weekends ----------------------------------------------------


sex_p_weekend_1 <- daily_crime_weekends %>% 
  fepois(sexual_assault ~ treatment | day_of_week + year + university,
        cluster = ~university, data = .) 
sex_p_weekend_2 <- daily_crime_weekends %>% 
  fepois(sexual_assault ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .) 
sex_p_weekend_3 <- daily_crime_weekends %>% 
  fepois(sexual_assault ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
sex_p_weekend_4 <- daily_crime_weekends %>% 
  fepois(sexual_assault ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .) 

# regressions weekdays ----------------------------------------------------


sex_p_weekdays_1 <- daily_crime_weekdays %>% 
  fepois(sexual_assault ~ treatment | day_of_week + year + university,
        cluster = ~university, data = .) 
sex_p_weekdays_2 <- daily_crime_weekdays %>% 
  fepois(sexual_assault ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .) 
sex_p_weekdays_3 <- daily_crime_weekdays %>% 
  fepois(sexual_assault ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
sex_p_weekdays_4 <- daily_crime_weekdays %>% 
  fepois(sexual_assault ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .) 



alcohol_sex_pois <- list("(1)" = alc_pois_1, "(2)" = alc_pois_2, "(3)" = alc_pois_3, "(4)" = alc_pois_4,
                        "(1)" = sex_pois_1, "(2)" = sex_pois_2, "(3)" = sex_pois_3, "(4)" = sex_pois_4)


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
initial_means_alc <- get_means(daily_crime, alcohol_offense)
initial_means_sex <- get_means(daily_crime, sexual_assault)

initial_means <- bind_cols(initial_means_alc, initial_means_sex) %>% 
  select(-`term...6`) %>% 
  rename("term" = "term...1")

## appending all the weekend columns together
models <- list(alc_p_weekend_1, alc_p_weekend_2, alc_p_weekend_3, alc_p_weekend_4,
               sex_p_weekend_1, sex_p_weekend_2, sex_p_weekend_3, sex_p_weekend_4)

## performing the get_est function on all the models, then combining them together and gettingn rid of the pvale
## I then append the means
weekend_means_alc <- get_means(daily_crime_weekends, alcohol_offense)
weekend_means_sex <- get_means(daily_crime_weekends, sexual_assault)
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
models_weekdays <- list(alc_p_weekdays_1, alc_p_weekdays_2, alc_p_weekdays_3, alc_p_weekdays_4,
                        sex_p_weekdays_1, sex_p_weekdays_2, sex_p_weekdays_3, sex_p_weekdays_4)

## similar to above
weekday_means_alc <- get_means(daily_crime_weekdays, alcohol_offense)
weekday_means_sex <- get_means(daily_crime_weekdays, sexual_assault)
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
main_regressions_pois <- modelsummary(alcohol_sex_pois,
                                 stars = T, 
                                 gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
                                 coef_map = c("treatment" = "Moratorium"),
                                 title = "\\label{main_pois}Effect of Moratoriums on Alcohol Offenses and Sexual Assault (Poisson).",
                                 notes = list("The sample includes 38 universities. Some universities go in and out of moratoriums multiple times.",
                                              "Standard errors are clustered by university.",
                                              "Outcomes of interest are alcohol offenses and reports of sexual assault counts.",
                                              "Coefficient estimates shown are for Moratorium."),
                                 add_rows = add_rows,
                                 output = "latex") %>% 
  pack_rows("Full Sample (Monday - Sunday)",1,4) %>% 
  pack_rows("Weekends (Friday - Sunday)", 5, 8) %>% 
  pack_rows("Weekdays (Monday - Thursday)",9, 12) %>% 
  add_header_above(c(" " = 1, "Alcohol Offense" = 4, "Sexual Assault" = 4)) %>% 
  kable_styling(latex_options = "scale_down")





