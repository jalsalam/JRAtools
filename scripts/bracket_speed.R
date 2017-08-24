# bracket_speed_test.R
# Speed testing "bracket in grouped mutate" on EMF data


library("tidyverse")
library("readxl")
library("microbenchmark")

##### Read Raw Data -------------
# Reads in .csv, gathers to a tall data format, and outputs emf_raw

# read CSV input file of modeling team submitted data
emf_raw1 <- read_csv(file = "data-raw/emf32r3v27_revrecyl_compare_20170707-145601.csv",                     
                     col_types = cols(
                       .default = col_double(),
                       MODEL    = col_character(),
                       SCENARIO = col_character(),
                       REGION   = col_character(),
                       VARIABLE = col_character(),
                       UNIT     = col_character()
                     ))

# converts data from individual year columns to a single column (tall format) and removes rows w/ NA values
emf_raw <- emf_raw1 %>%
  gather(YEAR, VALUE, `2005`:`2065`) %>%
  filter(!is.na(VALUE)) %>%
  mutate(YEAR = as.numeric(YEAR))

rm(emf_raw1)

#############

# Problem: get the "base year" data for every MODEL/SCENARIO/REGION/VARIABLE/UNIT grouping

# Function to encapsulate the join

bracket_with_join <- function(data) {
  df_by <- data %>%
    filter(YEAR == 2015) %>% # maybe a quosure goes where YEAR == 2015
    rename(VALUE.by = VALUE) %>% # would need a maybe temporary rename?
    select(-YEAR) # look at part of quosure to know what to remove?
  
  df_result <- data %>%
    left_join(df_by, by = c("MODEL", "SCENARIO", "REGION", "VARIABLE", "UNIT"))
  
  df_result
}



bracket_with_replacer <- function(data) {
  df_result <- data %>%
    group_by(MODEL, SCENARIO, REGION, VARIABLE, UNIT) %>%
    mutate(VALUE.by = (VALUE[YEAR == 2015] %|||% NA_real_))
  
  df_result
}

brack_with_fun_repl <- function(data) {
  df_result <- data %>%
    group_by(MODEL, SCENARIO, REGION, VARIABLE, UNIT) %>%
    mutate(VALUE.by = ret2(VALUE, YEAR == 2015))
  
  df_result
}

bracket_with_nth <- function(data) {
  df_result <- data %>%
    group_by(MODEL, SCENARIO, REGION, VARIABLE, UNIT) %>%
    mutate(VALUE.by = nth(VALUE, detect_index(YEAR, ~.x == 2015)))
  
  df_result
}

bracket_with_pluck <- function(data) {
  df_result <- data %>%
    group_by(MODEL, SCENARIO, REGION, VARIABLE, UNIT) %>%
    mutate(
      VALUE.by = pluck(VALUE,
                       detect_index(YEAR, ~.x == 2015),
                       .default = NA_real_))
  
  df_result
}

bracket_detect_ifs <- function(data) {
  df_result <- data %>%
    group_by(MODEL, SCENARIO, REGION, VARIABLE, UNIT) %>%
    mutate(VALUE.by = detect_ifs(VALUE, YEAR == 2015))
  
  df_result
}

# bracket_with_join is FAR faster than the others, and pluck is the slowest
microbenchmark(
  bracket_with_join(emf_raw),
  bracket_with_replacer(emf_raw),
  brack_with_fun_repl(emf_raw),
  times = 1L
)



