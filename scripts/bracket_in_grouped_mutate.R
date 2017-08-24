#bracket_within_grouped_mutate.R

#I commonly want to select across rows in a group. This is helpful for comparing the current year to the base year (e.g., making indices or percent changes), or when "VARIABLE" is stored in a column, doing math on several variables.

library("tidyverse")

###
# Setup -------
###

df <- tibble(
  name   = c("a",   "a", "b", "c", "c", "a", "a"),
  var    = c("hgt", "wt", "hgt", "wt", "hgt", "eye", "wt"),
  value  = c(72, 225, 64, 60, 142, 2, 210)
)

dfg <- df %>% group_by(name)

# Problem: how to calculate BMI = wt / hgt^2 for a, b, and c?

###
# How I want it to work, but it doensn't --------
###

df_bad_1 <- df %>%
  group_by(name) %>%
  summarize(BMI = value[var=="wt"] / value[var=="hgt"]^2)

# at core, need to make sure every call returns exactly one value (and NA if not found)

filter(df, name == "a") %>% {
  .$value[.$var == "wt"]
} # returns a value

filter(df, name == "b") %>% {
  .$value[.$var == "wt"]
} # but this returns an empty vector, which we'd prefer be NA

###
# Ways that don't work ----------
###

# if you use .$ its even worse: it loses the grouping info
df_bad_2 <- df %>%
  group_by(name) %>%
  summarize(ratio = value[.$var=="wt"]/ value[.$var=="hgt"]^2)

df_bad_again <- df %>%
  group_by(name) %>%
  summarize(wt = .data$value[.data$var=="wt"])

# if you use [[ it errors because trying to select 0
df_bad_3 <- df %>%
  group_by(name) %>%
  summarize(BMI = value[[var=="wt"]]/ value[[var=="hgt"]]^2)
# error msg says "more than one" but its really less...

####
# Ways that work --------------------
####

# Approach 1: long form with joins 
# Works on test data
# Results in NAs for missing (because left-join fills in NA for non-matching RHS)
# For multiple matches, it would give several rows, which is undesirable probably
# ALSO: it is ugly and long

approach_1 <- function(){

df_hgt <- df %>%
  filter(var == "hgt") %>%
  rename(hgt = value) %>%
  select(-var)

df_wt <- df %>%
  filter(var == "wt") %>%
  rename(wt = value) %>%
  select(-var)

df_good_but_long <- df %>%
  distinct(name) %>%
  left_join(df_hgt, by = "name") %>%
  left_join(df_wt, by = "name") %>%
  mutate(BMI = wt / hgt^2) %>%
  select(name, BMI)

rm(df_hgt, df_wt)
}



# Approach 2: make a helper function to replace empty list, then use `[` selection
# helper function variant on %||% from purrr that instead uses is_empty
# works on test data! For non-matches, `[` returns numeric(0), which is then replaced with NA_real_ by helper
# multiple matches would return multiple values, which would probably mess things up

`%|||%` <- function (x, y) 
{
  if (is_empty(x)) {
    y
  }
  else {
    x
  }
}

approach_2 <- function(){

df_replace_empty <- df %>%
  group_by(name) %>%
  summarize(ratio = (value[var=="wt"]  %|||% NA_real_) / 
                    (value[var=="hgt"] %|||% NA_real_)^2 )
}

# Approach 3: using dplyr::nth and purrr::detect_index
# detect_index returns 0 for the doesn't match case and only returns first for multiple match case
# nth returns NA for a zero index

detect_index(df$var, ~.x == "notthis")

approach_3 <- function() {

df_nth <- df %>% group_by(name) %>% 
  summarize(wt  = nth(value, detect_index(var, ~.x == "wt")),
            hgt = nth(value, detect_index(var, ~.x == "hgt")),
            BMI = hgt / wt ^2) %>%
  select(-wt, -hgt)
}



# Approach 4: using pluck instead of nth
# detect_index is still solving most of the problem
# pluck returns NULL for 0 index, but allows you to set the default

pluck(df$value, 0) # NULL
pluck(df$value, numeric(0)) # NULL

approach_4 <- function(){

df_pluck <- df %>%
  group_by(name) %>%
  summarize(
    wt = pluck(value, 
               detect_index(var, ~.x == "wt"), 
               .default = NA_real_),
    
    hgt = pluck(value, 
                detect_index(var, ~.x == "hgt"), 
                .default = NA_real_),
    
    BMI = hgt / wt^2)
}

microbenchmark(approach_1(),
               approach_2(),
               approach_3(),
               approach_4(),
               times = 100L)

# Approach 5: spread, calc, and gather
# in a sense doing a calculation over rows indicate that the data isn't "tidy", maybe?
# it seems that way with the "VARIABLE" variable, but less so with the year/base year example

# ...




# looking at the individual pieces -------------
# 1) a way to get the index we want
# 2) a way to retrieve the value at that index

## Step 1: Getting the index

# standard way:
idx_std <- dfg$var == "wt" # returns TRUEs and FALSEs
min(which(idx_std)) # can turn TRUEs and FALSEs into indices
idx_det <- detect_index(dfg$var, ~.x == "wt") #retuns the first index. returns 0 if not found

# now inside dplyr

idxs <- dfg %>% summarize(
  idx_std = list(var == "wt"),
  idx_whi = map_dbl(idx_std, .f = ~min(which(.))),
  idx_wh2 = map_dbl(list(var == "wt"), .f = ~min(which(.))),
  idx_det = detect_index(var, ~.x == "wt"), # Really, only this one works.
  idx = idx_det)

# Step 2: Retrieving by index (using detect_index index)

vals <- dfg %>% summarize(
  idx = detect_index(var, ~.x == "wt"),
  val_std = list(value[idx]),
  val_hlp = value[idx] %|||% NA_real_, #works
  val_nth = nth(value, idx), #works. preferred where you want to change the order.
  val_plu = pluck(value, idx, .default = NA_real_), #works. preferred for more complex predicate?
  val_pl2 = list(pluck(value, idx)),
  val_pl3 = pluck(value, idx)%||%  NA_real_
  )

# A big part of this is various ways index-getting functions say "I didn't find it" and the indexes the extracting funs handle

# `[`
df$value[0] # if passed 0, returns numeric(0)
df$value[NA_integer_] # if passed NA, returns NA!

# `[[`
df$value[[0]] # error
df$value[[NA_integer_]] # error

# nth
nth(df$value, 0) # returns NA!
nth(df$value, NA_integer_) # error
nth(df$value, NA_integer_, default = NA_real_) # still error
nth(df$value, Inf) # NA!
nth(df$value, NULL) # error
nth(df$value, numeric(0)) # error

# pluck
pluck(df$value, 0) # returns NULL
pluck(df$value, 0, .default = NA_real_) # NA !
pluck(df$value, NA_integer_) # NULL !
pluck(df$value, NA_real_, .default = NA_real_) # NA !
pluck(df$value, Inf, .default = NA_real_) # NA !
pluck(df$value, NULL, .default = NA_real_) # error
pluck(df$value, numeric(0), .default = NA_real_) # NA !

# detect
detect(df$value, 0) # NULL
detect(df$value, NA_integer_) # NULL
detect(df$value, NULL) # error
detect(df$value, Inf) # NULL


####
# Writing a function to encapsulate what I want in a shorter way -------------
#### 

# default_missing is in dplyr but not exported ... copying it here
default_missing <- function(x) {
  UseMethod("default_missing")
}

# default_missing(c(1)) %>% typeof()

default_missing.default <- function(x) {
  if (!is.object(x) && is.list(x)) {
    NULL
  } else {
    x[NA_real_]
  }
}

default_missing.data.frame <- function(x) {
  rep(NA, nrow(x))
}

such_that_1 <- function(x, .x, .p, ...) { # but I don't really like this syntax
  nth(x, detect_index(.x, .p, ...))
}

detect_if <- function(x, condition, .default = default_missing(x)) {
  
  idx <- detect_index(condition, isTRUE)
  val <- pluck(x, idx, .default = .default)
  val
}

`%st0%` <- detect_if

detect_ifs <- function(x, ...) {
  dots <- list(...)
  cond <- reduce(dots, `&`)
  
  idx <- detect_index(cond, isTRUE)
  val <- nth(x, idx)
  val
}

detect_ifs(df$value, df$var == "wt", df$name == "a")

df_myfun <- dfg %>%
  summarize(wt = such_that_1(value, var, ~.x == "wt"),
            nthif = detect_if(value, var == "wt"),
            st    = value %st0% c(var == "wt"),
            d_ifs = detect_ifs(value, var == "wt")
            )

nth(df$value, detect_index(df$var, ~.x == "wt"))
such_that(df$value, df$var, ~.x == "wt")

detect_index(df$var, "wt")

# Writing a function that does a filter/left-join... but this won't work inside summarize, which is why this has to be a vector function

retrieve <- function(data, x, cond, group_vars){
  x <- enquo(x)
  cond <- enquo(cond)
  
  df_which <- data %>%
    filter(!! cond) %>%
    rename(tempvarname = !! x) %>%
    select_at(vars(one_of(group_vars), tempvarname)) %>%
    group_by_at(group_vars) %>%
    slice(1) %>%
    ungroup()
  
  result <- data %>%
    left_join(df_which, by = group_vars) %>%
    pull(tempvarname)
}

df_retrieve <- df %>%
  mutate(wt = retrieve(df, value, var == "wt", group_vars = "name"))

df_retrieve <- df %>%
  group_by(name) %>%
  summarize(BMI = retrieve(., value, var == "wt") / 
              retrieve(., value, var == "hgt")^2)

ret2 <- function(x, cond) {
  x[cond] %|||% NA_real_
}

df %>%
  group_by(name) %>%
  summarize(wt = ret2(value, var == "wt"))

########## expressed in functional programming...

BMI <- function(hgt, wt) {
  hgt / wt^2
}

pluck_var <- function(var_val) {
  
  function(value, var) {
    pluck(value, detect_index(var, ~.x == var_val), .default = NA_real_)
  }
}

pluck_wt <- pluck_var("wt")
pluck_hgt <- pluck_var("hgt")


#####################3


# how do they work?
is_even <- function(x) x %% 2 == 0
x <- c(10, 11, 12, 13, 14)