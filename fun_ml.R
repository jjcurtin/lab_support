# Notes: This script includes functions that are useful for machine learning and other modeling and analysis projects

# Author:  John Curtin (jjcurtin@wisc.edu)


# Set conflict rules to resolve conflicts between tidymodels and tidyverse
tidymodels_conflictRules <- function(){
  conflictRules("scales", mask.ok = c("discard"), exclude = c("col_factor"))
  conflictRules("recipes", mask.ok = c("fixed"))
  conflictRules("yardstick", mask.ok = c("spec"))
}

# pull out a coefficient from a fitted parametric model
get_estimate <- function(the_fit, the_term){

  the_fit %>%
    broom::tidy() %>%
    dplyr::filter(term == the_term) %>%
    dplyr::pull(estimate)
}

# prep a recipe and bake some data
# By default, this function will glimpse your new features
# Set glimpse_it to FALSE to suppress this
make_features <- function(rec, data_trn, data_new = NULL, glimpse_it = TRUE){

  features <- rec %>%
    recipes::prep(training = data_trn, strings_as_factors = FALSE) %>%
    recipes::bake(new_data = data_new)

  if (glimpse_it){
    features %>% dplyr::glimpse()
  }

  return(features)
}

# makes a plot of observed (truth) vs estimate (predicted) plot
plot_truth <- function(truth, estimate) {

  ggplot2::ggplot(mapping = aes(x = truth, y = estimate)) +
    ggplot2::geom_abline(lty = 2) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::labs(y = "predicted outcome", x = "outcome") +
    ggplot2::coord_obs_pred()   # scale axes uniformly
}

# makes a hyperparameter plot for a model with up to 2 hyperparameters
plot_hyperparameters <- function(tune_fit, hp1, hp2 = NULL, metric = NULL, log_hp1 = FALSE) {

  data <- tune::collect_metrics(tune_fit)

  metric_scores <- data %>%
    dplyr::filter(.metric == metric) %>%
    dplyr::pull(mean)

  x1 <- data[[hp1]]
  if (log_hp1) x1 <- log(x1)

  if (is.null(hp2)) {
    ggplot2::ggplot(mapping = aes(x = x1, y = metric_scores)) +
      ggplot2::geom_line() +
      ggplot2::xlab(hp1) +
      ggplot2::ylab(metric)
  } else {
    x2 <- factor(data[[hp2]], ordered = TRUE)
    ggplot2::ggplot(mapping = aes(x = x1, y = metric_scores, group = x2, color = x2)) +
      ggplot2::geom_line() +
      ggplot2::xlab(hp1) +
      ggplot2::ylab(metric) +
      ggplot2::scale_color_discrete(name = hp2)
  }


}

#modified code from Kuhn described here: https://github.com/topepo/caret/issues/116
get_lambdas <- function(x, y, len = 50, model = "LASSO") {

  numLev <- if(is.character(y) | is.factor(y)) length(levels(y)) else NA

  if(!is.na(numLev)) {
    fam <- ifelse(numLev > 2, "multinomial", "binomial")
  } else fam <- "gaussian"

  if (toupper(model) == "LASSO"){
    alpha <- 1
  } else alpha <- 0

  init <- glmnet::glmnet(as.matrix(x), y,
                 family = fam,
                 nlambda = len+2,
                 alpha = alpha)
  lambda <- unique(init$lambda)
  lambda <- lambda[-c(1, length(lambda))]
  lambda <- lambda[1:min(length(lambda), len)]
}


# Nadeau and Bengio (2003) correlated t-test
nb_correlated_t_test <- function(cv_fits_full, cv_fits_compact, k = 10){

  cv_metrics_full <- tune::collect_metrics(cv_fits_full, summarize = FALSE)$.estimate
  cv_metrics_compact <- tune::collect_metrics(cv_fits_compact, summarize = FALSE)$.estimate
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
  tibble::tibble(mean_diff = mean_diff, se = se, t = t, df = n - 1, p_value = p_value)
}


# rope_min and rope_max are the lower and the upper bound of the rope
# if not NULL, plot_min and plot_max will return pdf for this range of metric diffs for plots
bayesian_correlated_t_test <- function(cv_fits_full, cv_fits_compact, rope_min, rope_max, k = 10, plot_min = NULL, plot_max = NULL, plot_n = 1000){
  if (rope_max < rope_min){
    stop("rope_max should be larger than rope_min")
  }

  cv_metrics_full <- tune::collect_metrics(cv_fits_full, summarize = FALSE)$.estimate
  cv_metrics_compact <- tune::collect_metrics(cv_fits_compact, summarize = FALSE)$.estimate
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

# Calculate variable importance for one variable or set of variables
# model is parsnip model
# x are features
# y is outcome (factor for classification)
# var_string is used to match to column names using contains(var_string)
# fun_metric is any metric function of similar form to the _vec functions in yardstick
#   e.g. roc_auc_vec, rmse_vec, accuracy_vec
# fun_pred is a prediction function that provides the appropriate estimate to
#   use with fun_metric.  See example below for roc_auc_vec
# n_reps is the number of permutations
# compare sets the order of contrast.  diff = metric - metrics_perm; 
#    diff_rev = metrics_perm - metric
# sample prediction function to use with yardstick::roc_auc_vec
#    predictor <- function(model, x){
#      predict(model, x, type = "prob") %>% 
#      pull(.pred_yes)  
#    }
get_vip <- function(model, x, y, var_string, fun_metric, fun_pred, n_reps = 20, 
                    compare = "diff"){
  
  metric <- fun_metric(truth = y, estimate = fun_pred(model, x))
  
  
  # requires foreach package attached in callng script
  metrics_perm <- foreach::foreach(rep = 1:n_reps, .combine='c') %do% {
    x %>% 
      dplyr::mutate(dplyr::across(dplyr::contains(var_string), sample)) %>% 
      fun_pred(model, .) %>% 
      fun_metric(truth = y, estimate = .)
  }
  
  if (!compare %in% c("diff", "diff_rev", "ratio", "ratio_rev")) {
    stop('Valid values for compare are "diff", "diff_rev"')
  }
  
  if(compare == "diff") {
    metrics_final <- metric - metrics_perm
  } else {
    metrics_final <- metrics_perm - metrics
  }
  
  
  vars <- x %>% 
    dplyr::select(contains(var_string)) %>% 
    names()
  
  vip <- tibble(var_string, vars = list(vars), 
                metric_mean = mean(metrics_final),
                metric_median = median(metrics_final),
                metric_05 = quantile(metrics_final, .05),
                metric_95 = quantile(metrics_final, .95))
  return(vip)
}
