# Notes: -------------------------------------------------------------------
# This script includes functions that are useful for machine learning and other modeling and analysis projects
# Functions are organized into separate sections for EDA, Model Building, and Model Evaluation
# Required packages are listed (and loaded) at the top if the script
# This script can be sourced to support your modeling efforts
# I am happy to receive feedback about or improvements to any of these functions.

# Author:  John Curtin (jjcurtin@wisc.edu)
# Updated: 2021-04-06

# Required libraries----------------------------------
library(tidyverse)
library(janitor)
library(cowplot)
library(kableExtra)
library(skimr) # for use of skim_with()
# library(e1071)  # for use of skewness(), kurtosis() in skim

# Exploratory Data Analysis-----------------------

# Provide summary statistics for cleaning EDA
skim_some <- skim_with(numeric = sfl(mean = NULL, sd = NULL, p25 = NULL, p50 = NULL, p75 = NULL, hist = NULL))

# Provides summary statistics for modeling EDA
skew_na <- partial(e1071::skewness, na.rm = TRUE)
kurt_na <- partial(e1071::kurtosis, na.rm = TRUE)
skim_all <- skim_with(numeric = sfl(skew = skew_na, kurtosis = kurt_na))


# provides simple table with counts and proportions
count_prop <- function(df, var, sort = FALSE) {
  df %>% count({{ var }}, sort = sort) %>% 
    mutate(prop = n / sum(n))
}


# Somewhat unformatted printing of text responses for categorical variables.
# Used primarily to confirm that responses are valid and tidy
print_responses <- function(name, column){
  unique(column) %>%
    na.omit() %>%
    str_c(collapse = ", ") %>%
    str_c(name, ": ", ., "\n") %>%
    cat()
}

# Used to tidy the responses/levels/labels for categorical variables
tidy_responses <- function(column){
  column <- factor(column)
  levels(column) <- janitor::make_clean_names(levels(column), use_make_names = FALSE) 
  as.character(column)
}

plot_freqpoly <- function(data, x, bins = 50){
  data %>%
    ggplot(aes(x = .data[[x]])) +
      geom_freqpoly(bins = bins) +
      theme(axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size = 11))
}

plot_bar <- function(data, x){
  x_label_size <- if_else(n_unique(data[[x]]) < 7, 11, 7)

  data %>%
    ggplot(aes(x = .data[[x]] )) +
    geom_bar() +
    theme(axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 11))
}

plot_box_violin <- function(data, x){
  x_label_size <- if_else(n_unique(data[[x]]) < 7, 11, 7)

  data %>%
    ggplot(aes(x = .data[[x]])) +
      geom_violin(aes(y = 0), fill = "green", color = NA) +
      geom_boxplot(width = .1, fill = NA, lwd = 1.1, fatten = 1.1) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1))
}

plot_boxplot <- function(data, x){
  x_label_size <- if_else(n_unique(data[[x]]) < 7, 11, 7)

  data %>%
    ggplot(aes(x = .data[[x]])) +
      geom_boxplot() +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1))
}

plot_tile <- function(data, x, y){
  data %>%
    count(.data[[x]], .data[[y]]) %>%
    ggplot(aes(x = .data[[x]], y = .data[[y]])) +
      geom_tile(mapping = aes(fill = n))
}

plot_hist <- function(data, x, bins = 100){
  data %>%
    ggplot(aes(x = .data[[x]])) +
      geom_histogram(bins = bins) +
      theme(axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size = 11))
}

plot_scatter <- function(data, x, y){
  data %>%
    ggplot(aes(x = .data[[x]], y = .data[[y]])) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, col = "red") +
      geom_smooth(method = "loess", formula = y ~ x, col = "green") +
      theme(axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size = 11))
}


plot_grouped_box_violin <- function(data, x, y){
  x_label_size <- if_else(n_unique(data[[x]]) < 7, 11, 7)

  data %>%
    ggplot(aes(x = .data[[x]], y = .data[[y]])) +
    geom_violin(fill = "green", color = NA) +
    geom_boxplot(width = .1, fill = NA, lwd = 1.1, fatten = 1.1) +
    theme(axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 11))
}


plot_hexbin <- function(data, x, y){
  data %>%
    ggplot(aes(x = .data[[x]], y = .data[[y]])) +
      geom_hex() +
      geom_smooth(method = "lm", col = "red") +
      geom_smooth(method = "loess", col = "green")  +
      theme(axis.text.x = element_text(size = 11),
            axis.text.y = element_text(size = 11))
}

plot_grouped_barplot_count <- function(data, x, y){
  x_label_size <- if_else(n_unique(data[[x]]) < 7, 11, 7)

  data %>%
    ggplot(aes(x = .data[[y]], fill = .data[[x]])) +
    geom_bar(position = "stack") +
    theme(axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 11))
}

plot_grouped_barplot_percent <- function(data, x, y){
  x_label_size <- if_else(n_unique(data[[x]]) < 7, 11, 7)

  data %>%
    ggplot(aes(x = .data[[y]], fill = .data[[x]])) +
      geom_bar(position = "fill") +
      labs(y = "Proportion") +
      theme(axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1),
            axis.text.y = element_text(size = 11))
}

plot_categorical <- function(data, x, y, ordered = FALSE){

  if (ordered) {
    data <- data %>%
      mutate(!!x := fct_reorder(.data[[x]],.data[[y]]))
  }

  x_label_size <- if_else(n_unique(data[[x]]) < 7, 11, 7)

  p_bar <- data %>%
    ggplot(aes(x = .data[[x]] )) +
    geom_bar()  +
    theme(axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 11))

  p_box <- data %>%
    ggplot(aes(x = .data[[x]], y = .data[[y]])) +
    geom_violin(fill = "green", color = NA) +
    geom_boxplot(width = .1, fill = NA, lwd = 1.1, fatten = 1.1) +
    theme(axis.text.x = element_text(angle = 90, size = x_label_size, vjust = 0.5, hjust = 1),
          axis.text.y = element_text(size = 11))

  return(list(p_bar, p_box))
}

## Primarily for Model Building-----------------------------

get_estimate <- function(the_fit, the_term){

  the_fit %>%
    tidy() %>%
    filter(term == the_term) %>%
    pull(estimate)
}

make_features <- function(rec, data_trn, data_new = NULL, glimpse_it = TRUE){

  features <- rec %>%
    prep(training = data_trn, strings_as_factors = FALSE) %>%
    bake(new_data = data_new)

  if (glimpse_it){
    features %>% glimpse()
  }

  return(features)
}

plot_truth <- function(truth, estimate) {

  ggplot(mapping = aes(x = truth, y = estimate)) +
    geom_abline(lty = 2) +
    geom_point(alpha = 0.5) +
    labs(y = "predicted outcome", x = "outcome") +
    coord_obs_pred()   # scale axes uniformly
}

plot_hyperparameters <- function(tune_fit, hp1, hp2 = NULL, metric = NULL, log_hp1 = FALSE) {

  data <- collect_metrics(tune_fit)

  metric_scores <- data %>%
    filter(.metric == metric) %>%
    pull(mean)

  x1 <- data[[hp1]]
  if (log_hp1) x1 <- log(x1)

  if (is.null(hp2)) {
    ggplot(mapping = aes(x = x1, y = metric_scores)) +
      geom_line() +
      xlab(hp1) +
      ylab(metric)
  } else {
    x2 <- factor(data[[hp2]], ordered = TRUE)
    ggplot(mapping = aes(x = x1, y = metric_scores, group = x2, color = x2)) +
      geom_line() +
      xlab(hp1) +
      ylab(metric) +
      scale_color_discrete(name = hp2)
  }


}



#modified code from Kuhn described here: https://github.com/topepo/caret/issues/116

get_lambdas <- function(x, y, len = 50, model = "LASSO") {
  require(glmnet)

  numLev <- if(is.character(y) | is.factor(y)) length(levels(y)) else NA

  if(!is.na(numLev)) {
    fam <- ifelse(numLev > 2, "multinomial", "binomial")
  } else fam <- "gaussian"

  if (toupper(model) == "LASSO"){
    alpha <- 1
  } else alpha <- 0

  init <- glmnet(as.matrix(x), y,
                 family = fam,
                 nlambda = len+2,
                 alpha = alpha)
  lambda <- unique(init$lambda)
  lambda <- lambda[-c(1, length(lambda))]
  lambda <- lambda[1:min(length(lambda), len)]
}


## Model Comparisons ------------------------------------------

# Nadeau and Bengio (2003) correlated t-test
nb_correlated_t_test <- function(cv_fits_full, cv_fits_compact, k = 10){

  cv_metrics_full <- collect_metrics(cv_fits_full, summarize = FALSE)$.estimate
  cv_metrics_compact <- collect_metrics(cv_fits_compact, summarize = FALSE)$.estimate
  diffs <- cv_metrics_full - cv_metrics_compact
  n <- length(diffs)
  mean_diff <- mean(diffs)
  var_diffs <- var(diffs)
  proportion_test <- 1 / k
  proportion_train <- 1 - proportion_test
  correction <- (1 / n) + (proportion_test / proportion_train)
  se = sqrt(correction * var_diffs)

  t = abs(mean_diff/se)
  p_value <- 2 * pt(t, n - 1, lower.tail = FALSE)
  tibble(mean_diff = mean_diff, se = se, t = t, df = n - 1, p_value = p_value)
}


# rope_min and rope_max are the lower and the upper bound of the rope
# if not NULL, plot_min and plot_max will return pdf for this range of metric diffs for plots
bayesian_correlated_t_test <- function(cv_fits_full, cv_fits_compact, rope_min, rope_max, k = 10, plot_min = NULL, plot_max = NULL, plot_n = 1000){
  if (rope_max < rope_min){
    stop("rope_max should be larger than rope_min")
  }

  cv_metrics_full <- collect_metrics(cv_fits_full, summarize = FALSE)$.estimate
  cv_metrics_compact <- collect_metrics(cv_fits_compact, summarize = FALSE)$.estimate
  diffs <- cv_metrics_full - cv_metrics_compact
  delta <- mean(diffs)
  n <- length(diffs)
  df <- n - 1
  stdX <- sd(diffs)
  rho = 1 / k
  sp <- sd(diffs)*sqrt(1/n + rho/(1-rho))
  p.left <- pt((rope_min - delta)/sp, df)
  p.rope <- pt((rope_max - delta)/sp, df)-p.left

  results <- list('left'=p.left,'rope'=p.rope,'right'=1-p.left-p.rope)

  if (!is.null(plot_min) & !is.null(plot_max)) {
    plot_diffs <- seq(plot_min, plot_max, length.out = plot_n)
    ts <- (plot_diffs - delta) / sp
    pdf <- dt(ts, df)
    results$plot_diffs <- plot_diffs
    results$pdf <- pdf
  }

  return(results)
}
