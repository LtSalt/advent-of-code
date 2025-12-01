

if(!"pacman" %in% installed.packages()) install.packages("pacman")
devtools::install_github("dgrtwo/adventdrob")
pacman::p_load(tidyverse, adventdrob)

input <- advent_input(1, 2025) %>% pull(x)

testvec <- c("L68", "L30", "R48", "L5", "R60", "L55", "L1", "L99", "R14", "L82")
testmoves <- testvec %>% str_replace_all(c("L" = "-", "R" = "+")) %>% as.numeric()


# One --------------------------------------------------------------------

solve1 <- function(input) {
  reducer <- function(acc, curr) {
    (acc + curr) %% 100
  }

  input %>% 
    str_replace_all(c("L" = "-", "R" = "+")) %>%
    as.numeric() %>% 
    accumulate(reducer, .init = 50) %>% 
    subset(. == 0) %>% 
    length()  
}

solve1(input)

# or (thx Opus)
moves <- input %>% str_replace_all(c("L" = "-", "R" = "+")) %>% as.numeric() 
positions <- cumsum(c(50, moves))
sum(positions %% 100 == 0)



# Two --------------------------------------------------------------------


solve2 <- function(positions) {
  reducer <- function(acc, curr) {
    acc <- c(head(acc, length(acc)-1),  tail(acc, 1):curr)
  }

  positions %>% 
    reduce(reducer) %>% 
    `%%`(100) %>% 
    subset(. == 0) %>% 
    length()  
}

solve2(positions)