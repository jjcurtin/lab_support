library(tidyverse)
theme_set(theme_classic()) 

fig_hist <- tibble(value = rnorm(100, 10, 1)) |> 
  ggplot(aes(x=value)) + 
    geom_histogram()