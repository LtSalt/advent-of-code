###########################################################################
# TITLE
###########################################################################


# Dependencies ------------------------------------------------------------

if(!"pacman" %in% installed.packages()) install.packages("pacman")
if("adventdrob" %in% installed.packages()) devtools::install_github("dgrtwo/adventdrob")
pacman::p_load(tidyverse, adventdrob)

input <- advent_input(2, 2025) %>% pull(x) %>% str_split_1(",")


# 01 ----------------------------------------------------------------------

parse_range <- function(idx_range) {
  from_to <- str_split_1(idx_range, "-")
  idx_vec <- as.character(seq(from_to[[1]], from_to[[2]], 1))
}

sum_vals <- function(somelist) {
  somelist %>% 
    unlist() %>% 
    as.numeric() %>% # `as.integer` will return some NAs, because R uses 32-bit signed integers, which max out at 2147483647
    sum()
}

is_invalid <- function(string) {
  str_sub(string, 1, nchar(string)/2) == str_sub(string, (nchar(string)/2) + 1, -1)
}

input %>% 
  map(parse_range) %>% 
  map(\(vec) keep(vec, is_invalid)) %>% 
  sum_vals()


# 02 ----------------------------------------------------------------------

divisors <- function(s) {
  n <- nchar(s)
  d <- which(n %% seq_len(n) == 0)
  d[d != n]
}

split_idx <- function(s, d) {
  starts <- seq(1, nchar(s), by = d)
  str_sub(s, starts, starts + d - 1)
}

is_invalid_2 <- function(s) {
  d <- divisors(s)
  
  for (x in d) {
    c <- split_idx(s, x)
    if (n_distinct(c) == 1) return(TRUE)
  }
  return(FALSE)
}

input %>% 
  map(parse_range) %>% 
  map(\(vec) keep(vec, is_invalid_2)) %>% 
  sum_vals()


# Notes -------------------------------------------------------------------

testinput <- c(
  "11-22",
  "95-115",
  "998-1012",
  "1188511880-1188511890",
  "222220-222224",
  "1698522-1698528",
  "446443-446449",
  "38593856-38593862",
  "565653-565659",
  "824824821-824824827",
  "2121212118-2121212124"
)

# Step 01 with self-implemented reducer

get_invalid_ids <- (function(vec) { # IIFEs for the win
  is_invalid <- function(string) {
    str_sub(string, 1, nchar(string)/2) == str_sub(string, (nchar(string)/2) + 1, -1)
  }
  
  reducer <- function(acc, curr) {
    if (is_invalid(curr)) return(c(acc, curr))
    acc
  }
  
  function(vec) {
    reduce(vec, reducer, .init = c())
  }  
})()

input %>% 
  map(parse_range) %>% 
  map(get_invalid_ids) %>% 
  sum_vals()


