###########################################################################
# 02
###########################################################################


# Dependencies ------------------------------------------------------------

if(!"pacman" %in% installed.packages()) install.packages("pacman")
if(!"adventdrop" %in% installed.packages()) devtools::install_github("dgrtwo/adventdrob")
pacman::p_load(tidyverse, adventdrob)

input <- advent_input(2, 2024) %>% 
  mutate(x = str_split(x, " ")) %>% 
  mutate(x = map(x, as.integer))


# 01 ----------------------------------------------------------------------

# naive way

sorted<- function(vec) {
  identical(vec, sort(vec)) | identical(vec, sort(vec,  decreasing = TRUE))
} 

distanced <- function(vec) {
  diffs <- abs(diff(vec))
  all(diffs >= 1 & diffs <= 3)
}

input %>% 
  mutate(sorted = map_lgl(x, sorted)) %>% 
  mutate(distanced = map_lgl(x, distanced)) %>% 
  filter(sorted, distanced) %>% 
  nrow()


# easier

input_vec <- input %>% pull(x)

is_safe <- function(vec) {
  diffs <- diff(vec)
  all(diffs >= 1 & diffs <= 3) | all(diffs >= -3 & diffs <= -1)  # check sorting by checking monotonicity in diffs
}

sum(map_lgl(input_vec, is_safe))


# 02 ----------------------------------------------------------------------

# expanding on first solution
is_safe_2 <- function(vec) {
  is_safe <- (sorted(vec) && distanced(vec)) # short-circuit to avoid unnecessary computation
  if (is_safe) return(TRUE)
  
  for (i in seq_along(vec)) {
    is_safe <- (sorted(vec[-i]) && distanced(vec[-i]))
    if (is_safe) return(TRUE)
  }
  
  return(is_safe)
}

sum(map_lgl(input_vec, is_safe_2))

# or (one liner, but less efficient)

is_safe_2 <- function(vec) {
  if (is_safe(vec)) return(TRUE)
  any(map_lgl(seq_along(vec), \(i) is_safe(vec[-i])))
}

sum(map_lgl(input_vec, is_safe_2))




