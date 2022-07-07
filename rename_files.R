# simple script to rename all files in a working directory
# first set working directory using setwd() or RStudio GUI

library(stringr)
library(tidyverse)

list.files() # to confirm that the correct files are in directory

names <- tibble(fn = list.files()) %>%   # could restrict list.files() to some pattern if needed
  separate(fn, c("stem", "ext"), "\\.") %>% 
  mutate(orig = str_c(stem, ".", ext),
         new = str_c(stem, "_b2.", ext)) %>% # change b2 as needed
  glimpse()

# rename files.  Should return TRUE for all files
file.rename(names$orig, names$new) %>% 
  sum()
