## Purpose of script:
##
## Author: Michael Topper
##
## Date Last Edited: 2021-10-11
##

library(tidyverse)
library(fixest)
library(modelsummary)
library(kableExtra)

if(!exists("daily_crime")) {
  daily_crime <- read_csv("Created Data/xMaster_data_2021/daily_panel.csv")
}

death_crime <- daily_crime %>% 
  filter(university %in% ifc::death_universities()) 

death_crime_weekends <- death_crime %>% 
  filter(day_of_week == 'Fri' | day_of_week == "Sat" | day_of_week == "Sun")

death_crime_weekdays <- death_crime %>% 
  filter(day_of_week == "Mon" | day_of_week == "Thu" | day_of_week == "Wed" | day_of_week == "Tue" )


# full panel --------------------------------------------------------------


x1 <- death_crime %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + year + university,
        cluster = ~university, data = .)

x2 <- death_crime %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .)

x3 <- death_crime %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
x4 <- death_crime %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .)


death_ols <- list("(1)" = x1, "(2)" = x1, "(3)" = x3, "(4)" = x4)



# weekends ----------------------------------------------------------------


x1_weekend <- death_crime_weekends %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + year + university,
        cluster = ~university, data = .)

x2_weekend <- death_crime_weekends %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .)

x3_weekend <- death_crime_weekends %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
x4_weekend <- death_crime_weekends %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .)


# weekdays ----------------------------------------------------------------


x1_weekday <- death_crime_weekdays %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + year + university,
        cluster = ~university, data = .)

x2_weekday <- death_crime_weekdays %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + year + university + semester_number,
        cluster = ~university, data = .)

x3_weekday <- death_crime_weekdays %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_semester_number + year ,
        cluster = ~university, data = .)
x4_weekday <- death_crime_weekdays %>% 
  feols(alcohol_offense_per25 ~ treatment | day_of_week + university_by_year_by_semester_number,
        cluster = ~university, data = .)

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
get_means <- function(x){
  mean <- x %>% 
    summarize(mean(alcohol_offense_per25, na.rm = T)) %>% 
    pull()
  mean_vector <- rep(mean, 5)
  mean_tibble <- tribble(~term, ~`est...2`, ~`est...3`, ~`est...4`, ~`est...5`,
                         "Mean of Dependent Variable", mean, mean, mean, mean)
  mean_tibble <- mean_tibble %>% 
    mutate(across(c(2:5), ~as.character(sprintf(.,fmt = "%.3f"))))
  return(mean_tibble)
}



## getting the dependent variable means
initial_means <- get_means(death_crime)

## appending all the weekend columns together
models <- list(x1_weekend, x2_weekend, x3_weekend, x4_weekend)

## performing the get_est function on all the models, then combining them together and gettingn rid of the pvale
## I then append the means
add_weekends <- map(models, ~get_est(.x)) %>% 
  reduce(bind_cols) %>% 
  reduce_frame() %>% 
  slice(-c(3)) %>% 
  bind_rows(get_means(death_crime_weekends))

## This is to add the means of the estimated model for the full sample first
add_weekends <- initial_means %>% 
  bind_rows(add_weekends)


## Now i do a similar thing with the weekday models
models_weekdays <- list(x1_weekday, x2_weekday, x3_weekday, x4_weekday)

## similar to above
add_weekdays <- map(models_weekdays, ~get_est(.x)) %>% 
  reduce(bind_cols) %>% 
  reduce_frame() %>% 
  slice(-c(3)) %>% 
  bind_rows(get_means(death_crime_weekdays))

## creating the final tibble to be added to the table
add_rows <- add_weekends %>% 
  bind_rows(add_weekdays)

## this line will change certain row terms
add_rows <- add_rows %>% 
  mutate(term = replace(term, term == "estimate", "Moratorium"),
         term = replace(term, term == "std.error", " "))

## adjusting where the position should be in the table
attr(add_rows, 'position') <- c(4:12)

death_alcohol <- modelsummary(death_ols,
                             stars = T, 
                             gof_omit = 'DF|Deviance|AIC|BIC|Log|R2',
                             coef_map = c("treatment" = "Moratorium"),
                             title = "\\label{death_regression}Effect of Moratoriums on Alcohol Offenses for Universities with Fraternity Deaths Only.",
                             notes = list("The sample includes 25 universities. 10 universities undergo moratoriums, while 15 do not.",
                                          "The 15 untreated universities experienced a fraternity-related death but no moratorium.",
                                          "Standard errors are clustered by university.",
                                          "Outcome of interest is alcohol offenses per 25 thousand students.",
                                          "Coefficient estimates shown are for Moratorium.",
                                          "Full Sample includes only academic calendar days (plus 1 extra week on each end)."),
                             add_rows = add_rows,
                             output = "latex") %>% 
  pack_rows("Full Sample (Monday - Sunday)",1,4) %>% 
  pack_rows("Weekends (Friday - Sunday)", 5, 8) %>% 
  pack_rows("Weekdays (Monday - Thursday)",9, 12)



