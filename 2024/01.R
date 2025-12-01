##########################################################################
# 01
##########################################################################


# Dependencies -----------------------------------------------------------

if(!"pacman" %in% installed.packages()) install.packages("pacman")
if(!"adventdrop" %in% installed.packages()) devtools::install_github("dgrtwo/adventdrob")
pacman::p_load(tidyverse, adventdrob)

input <- advent_input(1, 2024) %>% 
  mutate(x = trimws(x)) %>% 
  separate(x, c("x", "y"), sep = " +") %>% 
  mutate(across(everything(), as.numeric))

# 01 ---------------------------------------------------------------------

input %>% 
  mutate(across(everything(), \(col) sort(col))) %>% 
  mutate(diff = abs(x - y)) %>% 
  summarise(sum = sum(diff)) %>% 
  pull(sum)
  

# 02 ---------------------------------------------------------------------

input %>% 
  mutate(count = map(x, \(val) y == val)) %>% 
  mutate(count = map_int(count, sum)) %>% 
  reframe(vals = x * count) %>% 
  reframe(sum = sum(vals)) %>% 
  pull(sum)