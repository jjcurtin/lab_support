make_splits <- function(d, cv_resample_type, cv_resample = NULL, cv_outer_resample = NULL, cv_inner_resample = NULL, cv_group = NULL, the_seed = NULL) {
  
  # d: (training) dataset to be resampled 
  # cv_resample_type: can be boot, kfold, or nested
  # resample: specifies for repeats and folds for CV (1_x_10; 10_x_10) or num splits for bootstrapping (100)
  # inner_resample: specifies repeats/folds or num bootstrap splits for nested cv inner loop - same format as above
  # outer_resample: specifies repeats/folds for outer nested cv loop - cannot use bootstrapping here
  # group: specifies grouping variable for grouped cv and nested cv
  
  if(is.null(the_seed)) {
    error("make_splits() requires a seed")
  } else set.seed(the_seed)
  
  # bootstrap splits
  if (cv_resample_type == "boot") {
    splits <- d %>% 
      bootstraps(times = cv_resample)
  }

  
  # kfold - includes grouped, and repeated kfold
  if (cv_resample_type == "kfold") {
    # get number of repeats and folds
    n_repeats <- as.numeric(str_remove(cv_resample, "_x_\\d{1,2}"))
    n_folds <- as.numeric(str_remove(cv_resample, "\\d{1,3}_x_"))
    
    if (!is.null(cv_group)) {
      splits <- d %>% 
        group_vfold_cv(v = n_folds, repeats = n_repeats, group = all_of(cv_group)) 
    } else {
      splits <- d %>% 
        vfold_cv(v = n_folds, repeats = n_repeats) 
    }
  }
 
  
  # nested   
  if (cv_resample_type == "nested") {
    # get number of repeats and folds for outer cv loop
    outer_n_repeats <- as.numeric(str_remove(cv_outer_resample, "_x_\\d{1,2}"))
    outer_n_folds <- as.numeric(str_remove(cv_outer_resample, "\\d{1,3}_x_"))
    
    # get repeats/folds or bootstrap splits for inner loop
    if (str_detect(cv_inner_resample, "_x_")) {
      inner_n_repeats <- as.numeric(str_remove(cv_inner_resample, "_x_\\d{1,2}"))
      inner_n_folds <- as.numeric(str_remove(cv_inner_resample, "\\d{1,3}_x_"))
    } else {
      inner_boot_splits <- cv_inner_resample
    }

    
    # create splits for grouped nested cv (requires inner and outer to be kfold)
    if (!is.null(cv_group)) {
      # needed to create outer folds outside of nested_cv for some unknown reason!
      outer_grouped_kfold <- d %>% 
        group_vfold_cv(v = outer_n_folds, repeats = outer_n_repeats, 
                       group = all_of(cv_group))
      splits <- d %>% 
        nested_cv(outside = outer_grouped_kfold, 
                  inside = group_vfold_cv(v = inner_n_folds, 
                                          repeats = inner_n_repeats, 
                                          group = all_of(cv_group)))
    } 
    
    # create splits for ungrouped nested cv with kfold inner
    if (is.null(cv_group) & str_detect(cv_inner_resample, "_x_")) {

      splits <- d %>% 
        nested_cv(outside = vfold_cv(v = outer_n_folds, repeats = outer_n_repeats) , 
                  inside = vfold_cv(v = inner_n_folds, repeats = inner_n_repeats))
    }
    
    # create splits for ungrouped nested cv with bootstrapping inner
    # not to be used when grouping
    if (is.null(cv_group) & !str_detect(cv_inner_resample, "_x_")) {
      
      splits <- d %>% 
        nested_cv(outside = vfold_cv(v = outer_n_folds, repeats = outer_n_repeats) , 
                  inside = bootstraps(times = inner_boot_splits))
    }
  }
    
    
    # Demo code for extracting inner and outer cv resamples
    # get train/test for outer loop fold 1
    # out1_train <- training(splits$splits[[1]]) %>% 
    #   glimpse
    # 
    # out1_test <- testing(splits$splits[[1]]) %>% 
    #   glimpse
    # 
    # # get inner splits for outer loop fold 1
    # out1_inners <- splits$inner_resamples[[1]] %>% 
    #   glimpse
    # 
    # # get inner train/test fold 1 for outer fold 2
    # out2_inner1_train <- training(splits$inner_resamples[[2]]$splits[[1]]) %>% 
    #   glimpse
    # 
    # out2_inner1_test <- testing(splits$inner_resamples[[2]]$splits[[1]]) %>% 
    #  glimpse
  
  return(splits)
}

make_rset <- function(splits, cv_resample_type, split_num = NULL, 
                      inner_split_num = NULL, outer_split_num = NULL) {
# used to make an rset object that contains a single split for use in 
# tuning glmnet on CHTC  
  
  if (cv_resample_type == "nested") {
    split <- splits$inner_resamples[[outer_split_num]] %>% 
      slice(inner_split_num) 
  }
  
  if (cv_resample_type == "kfold") {
    split <- splits %>% 
      slice(split_num)
  }

  if (cv_resample_type == "boot") {
    stop("Make rset does not work for bootstrap resamples")
  }
  
  rset <- manual_rset(split$splits, split$id)
  return(rset)
}

tune_model <- function(config, rec, splits, ml_mode, cv_resample_type, hp2_glmnet_min = NULL,
                       hp2_glmnet_max = NULL, hp2_glmnet_out = NULL, y_level_pos = NULL) {
  # config: single-row config-specific tibble from jobs
  # splits: rset object that contains all resamples
  # rec: recipe (created manually or via build_recipe() function)
  
  # set metrics for regression or classification
  if (ml_mode == "regression") {
    mode_metrics <- metric_set(rmse, rsq)
  }
  
  if (ml_mode == "classification") {
    mode_metrics <- metric_set(accuracy, bal_accuracy, roc_auc,
                               sens, yardstick::spec, ppv, npv)
  }
  
  
  
  if (config$algorithm == "glmnet") {
    grid_penalty <- expand_grid(penalty = exp(seq(hp2_glmnet_min, hp2_glmnet_max, 
                                                  length.out = hp2_glmnet_out)))
    
    # make rset for single held-in/held_out split
    # does not work for bootstrapping
    split <- make_rset(splits, cv_resample_type = cv_resample_type, 
                       split_num = config$split_num, 
                       inner_split_num = config$inner_split_num, 
                       outer_split_num = config$outer_split_num)
    
    if (ml_mode == "classification") {
      models <- logistic_reg(penalty = tune(),
                             mixture = config$hp1) %>%
        set_engine("glmnet") %>%
        set_mode("classification") %>%
        tune_grid(preprocessor = rec,
                  resamples = split,
                  grid = grid_penalty,
                  # metrics assume that positive event it first level
                  # make sure this is true in recipe
                  metrics = mode_metrics)
    } else {
      models <- linear_reg(penalty = tune(),
                           mixture = config$hp1) %>%
        set_engine("glmnet") %>%
        set_mode("regression") %>%
        tune_grid(preprocessor = rec,
                  resamples = split,
                  grid = grid_penalty,
                  # metrics assume that positive event it first level
                  # make sure this is true in recipe
                  metrics = mode_metrics)
      
      
    }
    # create tibble of penalty and metrics returned 
    results <- collect_metrics(models, summarize = FALSE) %>% 
      rename(hp2 = penalty) %>% 
      select(hp2, .metric, .estimate) %>% 
      pivot_wider(., names_from = ".metric",
                  values_from = ".estimate") %>%  
      bind_cols(config %>% select(-hp2), .) %>% 
      relocate(hp2, .before = hp3) 
    
    return(results)
  }
  
  if (config$algorithm == "random_forest") {
    # extract fold associated with this config - 1 held in and 1 held out set and make 1 
    # set of features for the held in and held out set 
    features <- make_config_features(config = config, splits = splits, rec = rec, 
                              cv_resample_type = cv_resample_type)
    feat_in <- features$feat_in
    feat_out <- features$feat_out
    
    # fit model on feat_in with config hyperparameter values 
    model <- rand_forest(mtry = config$hp1,
                         min_n = config$hp2,
                         trees = config$hp3) %>%
      set_engine("ranger",
                 importance = "none",
                 respect.unordered.factors = "order",
                 oob.error = FALSE,
                 seed = 102030) %>%
      set_mode(ml_mode) %>%
      fit(y ~ .,
          data = feat_in)
    
    # use get_metrics function to get a tibble that shows performance metrics
    if (ml_mode == "classification") {
      results <- get_metrics(model = model, feat_out = feat_out, ml_mode, 
                             y_level_pos) %>% 
        pivot_wider(., names_from = "metric",
                    values_from = "estimate") %>%   
        relocate(sens, spec, ppv, npv, accuracy, bal_accuracy, roc_auc) %>% 
        bind_cols(config, .) 
    } else {
      results <- get_metrics(model = model, feat_out = feat_out, ml_mode,
                             y_level_pos) %>% 
        bind_cols(config, .) 
    }
    
    return(results)
  }
  
  if (config$algorithm == "xgboost") {
    
    # extract fold associated with this config - 1 held in and 1 held out set and make 1 
    # set of features for the held in and held out set 
    features <- make_config_features(config = config, splits = splits, rec = rec, 
                              cv_resample_type = cv_resample_type)
    feat_in <- features$feat_in
    feat_out <- features$feat_out
    
    # fit model on feat_in with config hyperparameter values 
    model <- boost_tree(learn_rate = config$hp1,
                        tree_depth = config$hp2,
                        mtry = config$hp3,
                        trees = 500,  # set high but use early stopping
                        stop_iter = 20) %>% 
      set_engine("xgboost",
                 validation = 0.2) %>% 
      set_mode(ml_mode) %>%
      fit(y ~ ., data = feat_in)
    
    # use get_metrics function to get a tibble that shows performance metrics
    if (ml_mode == "classification") {
      results <- get_metrics(model = model, feat_out = feat_out, ml_mode,
                             y_level_pos) %>% 
        pivot_wider(., names_from = "metric",
                    values_from = "estimate") %>%   
        relocate(sens, spec, ppv, npv, accuracy, bal_accuracy, roc_auc) %>% 
        bind_cols(config, .) 
    } else {
      results <- get_metrics(model = model, feat_out = feat_out, ml_mode,
                             y_level_pos) %>% 
        bind_cols(config, .) 
    }
    
    return(results)
  }
  
  if (config$algorithm == "knn") {
    # extract single fold associated with config
    features <- make_config_features(config = config, splits = splits, rec = rec, 
                              cv_resample_type = cv_resample_type)
    feat_in <- features$feat_in
    feat_out <- features$feat_out
    
    # fit model - config provides number of neighbors
    model <- nearest_neighbor(neighbors = config$hp1) %>% 
      set_engine("kknn") %>% 
      set_mode(ml_mode) %>% 
      fit(y ~ .,
          data = feat_in)
    
    # use get_metrics function to get a tibble that shows performance metrics
    if (ml_mode == "classification") {
      results <- get_metrics(model = model, feat_out = feat_out, ml_mode,
                             y_level_pos) %>% 
        pivot_wider(., names_from = "metric",
                    values_from = "estimate") %>%   
        relocate(sens, spec, ppv, npv, accuracy, bal_accuracy, roc_auc) %>% 
        bind_cols(config, .) 
    } else {
      results <- get_metrics(model = model, feat_out = feat_out, ml_mode,
                             y_level_pos) %>% 
        bind_cols(config, .) 
    }
    
    return(results) 
  }
  
  if (config$algorithm == "glm") {
    # extract fold associated with this config - 1 held in and 1 held out set and make 1 
    # set of features for the held in and held out set 
    features <- make_config_features(config = config, splits = splits, rec = rec, 
                                     cv_resample_type = cv_resample_type)
    feat_in <- features$feat_in
    feat_out <- features$feat_out
    
    # fit model on feat_in with config hyperparameter values 
    model <- logistic_reg() %>% 
      set_engine("glm") %>% 
      set_mode(ml_mode) %>% 
      fit(y ~ .,
          data = feat_in)
    
    # use get_metrics function to get a tibble that shows performance metrics
    if (ml_mode == "classification") {
      results <- get_metrics(model = model, feat_out = feat_out, ml_mode, 
                             y_level_pos) %>% 
        pivot_wider(., names_from = "metric",
                    values_from = "estimate") %>%   
        relocate(sens, spec, ppv, npv, accuracy, bal_accuracy, roc_auc) %>% 
        bind_cols(config, .) 
    } else {
      results <- get_metrics(model = model, feat_out = feat_out, ml_mode,
                             y_level_pos) %>% 
        bind_cols(config, .) 
    }
  }
}


# helper function for tune_model()
# KW: still need to add section for bootstrap
make_config_features <- function(config, splits, rec, cv_resample_type) {
  

  # config: single-row config-specific tibble
  # splits: rset object that contains all resamples
  # rec: recipe (created manually or via build_recipe() function)
  # cv_resample_type: either boot, kfold, or nested
  
  if (cv_resample_type == "kfold") {
    
    d_in <- training(splits$splits[[config$split_num]])
    d_out <- testing(splits$splits[[config$split_num]])
  }
  
  
  if (cv_resample_type == "nested") {
    
    d_in <- training(splits$inner_resamples[[config$outer_split_num]]$splits[[config$inner_split_num]])

    d_out <- testing(splits$inner_resamples[[config$outer_split_num]]$splits[[config$inner_split_num]]) 
  }
  
  if (cv_resample_type == "boot") {
   
    # pull out bootstrap split here
    
  }
  
  # make feature matrices
  feat_in <- rec %>% 
    prep(training = d_in, strings_as_factors = FALSE) %>% 
    bake(new_data = NULL)
  
  feat_out <- rec %>% 
    prep(training = d_in, strings_as_factors = FALSE) %>% 
    bake(new_data = d_out)
  
  return(list(feat_in = feat_in, feat_out = feat_out))
  
}

# helper function for tune_model()
get_metrics <- function(model, feat_out, ml_mode, y_level_pos) {
  
  # model: single model object 
  # feat_out: feature matrix built from held-out data
  
  
  if (ml_mode == "classification") {
    preds <- predict(model, feat_out, type = "class")$.pred_class
    
    cm <- tibble(truth = feat_out$y,
                 estimate = preds) %>% 
      conf_mat(truth, estimate)
    
    model_metrics <- cm %>% 
      summary(event_level = "first") %>%   # make sure this is true in recipe
      select(metric = .metric,
             estimate = .estimate) %>% 
      filter(metric %in% c("sens", "spec", "ppv", "npv", "accuracy", "bal_accuracy")) %>% 
      suppressWarnings() # warning not about metrics we are returning
    
    roc <- tibble(truth = feat_out$y,
                  prob = predict(model, feat_out,
                                type = "prob")[[str_c(".pred_", y_level_pos)]]) %>% 
      roc_auc(prob, truth = truth, event_level = "first") %>% 
      select(metric = .metric, 
             estimate = .estimate)
    
    model_metrics <- bind_rows(model_metrics, roc)
  }
  
  if (ml_mode == "regression") {
    
    preds <- predict(model, feat_out)$.pred
    rmse_model <- rmse_vec(truth = feat_out$y, estimate = preds)
    rsq_model <- rsq_vec(truth = feat_out$y, estimate = preds)
   
    
    model_metrics <- tibble(rsq = rsq_model, rmse = rmse_model)
  }
  
  return(model_metrics)
}

eval_best_model <- function(config_best, rec, splits, ml_mode) {
# evaluates best model configuration using resamples of data contained in splits.
  
  # specific setup for regression or classification
  if (ml_mode == "regression") {
    mode_metrics <- metric_set(rmse, rsq)
    # control grid to save predictions
    ctrl <- control_resamples(save_pred = TRUE, 
                              extract = function (x) extract_fit_parsnip(x) %>% tidy())
    # add resample to best_config to make consistent with classification
    config_best <- config_best %>% 
      mutate(resample = NA)
  }
  
  if (ml_mode == "classification") {
    mode_metrics <- metric_set(accuracy, bal_accuracy, roc_auc,
                               sens, yardstick::spec, ppv, npv)
    # control grid to save predictions
    ctrl <- control_resamples(save_pred = TRUE, 
                              event_level = "first",
                              extract = function (x) extract_fit_parsnip(x) %>% tidy())
  }
  
  
  if (config_best$algorithm == "glmnet") {
    
    # Doing branch because need logistic_reg vs. linear_reg
    # Can fix later with more generic code
    if (ml_mode == "classification") {
      models <- logistic_reg(penalty = config_best$hp2,
                            mixture = config_best$hp1) %>%
        set_engine("glmnet") %>%
        set_mode(ml_mode) %>%
        fit_resamples(preprocessor = rec,
                      resamples = splits,
                      metrics = mode_metrics,
                      control = ctrl)
      } else {
        models <- linear_reg(penalty = config_best$hp2,
                               mixture = config_best$hp1) %>%
          set_engine("glmnet") %>%
          set_mode(ml_mode) %>%
          fit_resamples(preprocessor = rec,
                        resamples = splits,
                        metrics = mode_metrics,
                        control = ctrl)
      }
  }

  
  if (config_best$algorithm == "random_forest") {
    
    # fit model on feat_in with config_best hyperparameter values 
    models <- rand_forest(mtry = config_best$hp1,
                         min_n = config_best$hp2,
                         trees = config_best$hp3) %>%
        set_engine("ranger",
                   importance = "none",
                   respect.unordered.factors = "order",
                   oob.error = FALSE,
                   seed = 102030) %>%
        set_mode(ml_mode) %>%
        fit_resamples(preprocessor = rec,
                      resamples = splits,
                      metrics = mode_metrics,
                      control = ctrl)
  }

  
  if (config_best$algorithm == "xgboost") {
    
    # fit model on feat_in with config_best hyperparameter values 
    models <- boost_tree(learn_rate = config_best$hp1,
                        tree_depth = config_best$hp2,
                        mtry = config_best$hp3,
                        trees = 500,  # set high but use early stopping
                        stop_iter = 20) %>% 
      set_engine("xgboost",
                 validation = 0.2) %>% 
      set_mode(ml_mode) %>%
      fit_resamples(preprocessor = rec,
                    resamples = splits,
                    metrics = mode_metrics,
                    control = ctrl)
  }
  
  
  if (config_best$algorithm == "knn") {
    
    # fit model - config_best provides number of neighbors
    
    models <- nearest_neighbor(neighbors = config_best$hp1) %>% 
      set_engine("kknn") %>% 
      set_mode(ml_mode) %>% 
      fit_resamples(preprocessor = rec,
                    resamples = splits,
                    metrics = mode_metrics,
                    control = ctrl)
  }
    
    results <- collect_metrics(models, summarize = FALSE) %>%
      pivot_wider(., names_from = ".metric",
                  values_from = ".estimate") %>%
      select(-.estimator) %>% 
      bind_cols(config_best %>% select(algorithm, feature_set, hp1, hp2, hp3, resample), .)

    
    
    # Create a tibble of predictions
    predictions <- collect_predictions(models)
    
    return(list(results, predictions, models))

}

fit_best_model <- function(best_model, rec, d, ml_mode) {
  
  
  # make features for full dataset
  feat <- rec %>% 
    prep(training = d, strings_as_factors = FALSE) %>% 
    bake(new_data = NULL)
  
  if (best_model$algorithm == "glmnet") {
    
    if (ml_mode == "classification") {
      fit_best <- logistic_reg(penalty = best_model$hp2,
                             mixture = best_model$hp1) %>%
        set_engine("glmnet") %>%
        set_mode(ml_mode) %>%
        fit(y ~ ., data = feat)
    } else {
      fit_best <- linear_reg(penalty = best_model$hp2,
                               mixture = best_model$hp1) %>%
        set_engine("glmnet") %>%
        set_mode(ml_mode) %>%
        fit(y ~ ., data = feat)     
    }
    
    return(fit_best)
  }
  
  if (best_model$algorithm == "random_forest") {
    
    fit_best <- rand_forest(mtry = best_model$hp1,
                          min_n = best_model$hp2,
                          trees = best_model$hp3) %>%
      set_engine("ranger",
                 importance = "impurity",
                 respect.unordered.factors = "order",
                 oob.error = FALSE,
                 seed = 102030) %>%
      set_mode(ml_mode) %>%
      fit(y ~ ., data = feat)
    
    
    return(fit_best)
  }
  
  if (best_model$algorithm == "xgboost") {
    
    fit_best <- boost_tree(learn_rate = best_model$hp1,
                           tree_depth = best_model$hp2,
                           mtry = best_model$hp3,
                           trees = 500,  # set high but use early stopping
                           stop_iter = 20) %>% 
      set_engine("xgboost",
                 validation = 0.2) %>% 
      set_mode(ml_mode) %>%
      fit(y ~ ., data = feat)
    
    return(fit_best)
  }
  
  if (best_model$algorithm == "knn") {
    
    fit_best <- nearest_neighbor(neighbors = best_model$hp1) %>% 
      set_engine("kknn") %>% 
      set_mode(ml_mode) %>% 
      fit(y ~ ., data = feat)
    
    return(fit_best)
  }
}

