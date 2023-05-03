# These are simply plotting functions that make it easier 
# to do visual EDA.  They are used extensively in Psy 752 (Intro to Applied Machine Learning) but also by our lab.

# Author:  John Curtin (jjcurtin@wisc.edu)

# requires tidyverse to be attached in main script


plot_freqpoly <- function(x, bins = 50){
  tibble(x) |>
    ggplot(aes(x = x)) +
    geom_freqpoly(bins = bins) +
    theme(axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11))
}

plot_bar <- function(x){
  x_label_size <- if_else(skimr::n_unique(x) < 7, 11, 7)
  
  tibble(x) |>
    ggplot(aes(x = x)) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 11))
}

plot_box_violin <- function(x){
  x_label_size <- if_else(skimr::n_unique(x) < 7, 11, 7)
  
  tibble(x) |>
    ggplot(aes(x = x)) +
    geom_violin(aes(y = 0), fill = "green", color = NA) +
    geom_boxplot(width = .1, fill = NA, lwd = 1.1, fatten = 1.1) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1))
}

plot_boxplot <- function(x){
  x_label_size <- if_else(skimr::n_unique(x) < 7, 11, 7)
  
  tibble(x) |>
    ggplot(aes(x = x)) +
    geom_boxplot() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1))
}

plot_tile <- function(x, y){
  tibble(x, y) |>
    count(x, y) |>
    ggplot(aes(x = x, y = y)) +
    geom_tile(mapping = aes(fill = n))
}

plot_hist <- function(x, bins = 100){
  tibble(x) |>
    ggplot(aes(x = x)) +
    geom_histogram(bins = bins) +
    theme(axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11))
}

plot_scatter <- function(x, y){
  tibble(x, y) |>
    ggplot(aes(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x, col = "red") +
    geom_smooth(method = "loess", formula = y ~ x, col = "green") +
    theme(axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11))
}


plot_grouped_box_violin <- function(x, y){
  x_label_size <- if_else(skimr::n_unique(x) < 7, 11, 7)
  
  tibble(x, y) |>
    ggplot(aes(x = x, y = y)) +
    geom_violin(fill = "green", color = NA) +
    geom_boxplot(width = .1, fill = NA, lwd = 1.1, fatten = 1.1) +
    theme(axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 11))
}


plot_hexbin <- function(x, y){
  tibble(x, y) |>
    ggplot(aes(x = x, y = y)) +
    geom_hex() +
    geom_smooth(method = "lm", col = "red") +
    geom_smooth(method = "loess", col = "green")  +
    theme(axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11))
}

plot_grouped_barplot_count <- function(x, y){
  x_label_size <- if_else(skimr::n_unique(x) < 7, 11, 7)
  
  tibble(x, y) |>
    ggplot(aes(x = y, fill = x)) +
    geom_bar(position = "stack") +
    theme(axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 11))
}

plot_grouped_barplot_percent <- function(x, y){
  x_label_size <- if_else(skimr::n_unique(x) < 7, 11, 7)
  
  tibble(x, y) |>
    ggplot(aes(x = y, fill = x)) +
    geom_bar(position = "fill") +
    labs(y = "Proportion") +
    theme(axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 11))
}

plot_categorical <- function(x, y, ordered = FALSE){
  
  df <- tibble(x, y)
  if (ordered) {
    df <- df |>
      #mutate(!!x := fct_reorder({{ x }},{{ y }}))
      mutate(x = fct_reorder(x, y))
  }
  
  x_label_size <- if_else(skimr::n_unique(x) < 7, 11, 7)
  
  p_bar <- df |>
    ggplot(aes(x = x )) +
    geom_bar()  +
    theme(axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 11))
  
  p_box <- df |>
    ggplot(aes(x = x, y = y)) +
    geom_violin(fill = "green", color = NA) +
    geom_boxplot(width = .1, fill = NA, lwd = 1.1, fatten = 1.1) +
    theme(axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 11))
  
  return(list(p_bar, p_box))
}