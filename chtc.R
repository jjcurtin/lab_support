suppressPackageStartupMessages({
  require(dplyr)
  require(recipes)
  require(parsnip)
  require(themis)
  require(tune)
  require(yardstick)
  require(rsample)
  require(ranger)
  require(psych)
  require(purrr)
  require(glmnet)
  # require(kknn)
  require(vip)
})


# JC general notes.  Need to make assumption about outcome name or else pass it in as string
# Can also have a function that takes all results and selects best model configuration





# splits for grouped repeated k-fold (subid = grouping factor)
# JC FUNCTION NOTE: Don't need job because the resampling technique should be the same for
# all jobs.  Do need to specify resampling technique as parameter(s) but likely as a context
# variable in the calling script
# Should support repeated kfold, repeated grouped kfold and bootstrap

make_folds <- function(d, job) {
  
  # d: (training) dataset to be resampled (subid = grouping id)
  # job: single job to get n_repeats and n_folds from cv_type
  
  n_repeats <- as.numeric(str_split(job$cv_type, "_x_")[[1]][1])
  n_folds <- as.numeric(str_split(job$cv_type, "_x_")[[1]][2])
  
  for (i in 1:n_repeats) {
    fold <- d %>% 
      group_vfold_cv(group = subid, v = n_folds) %>% 
      mutate(n_repeat = i)
    
    folds <- if (i == 1)
      fold
    else
      rbind(folds, fold)
  }
  
  return(folds)
}


# JC FUNCTION NOTE.  I dont think this one can be generic. Lets discuss

build_recipe <- function(d, job) {
  
  # d: (training) dataset from which to build recipe
  # job: single-row job-specific tibble
  # lapse = outcome variable (lapse/no lapse)
  # feature_set = all_features or passive_only
  # resample = none, up, down, or smote
  
  algorithm <- job$algorithm
  feature_set <- job$feature_set
  resample <- job$resample
  
  rec <- recipe(lapse ~ ., data = d) %>%
    step_string2factor(lapse, levels = c("no", "yes")) %>% 
    update_role(subid, dttm_label, new_role = "id variable") %>%
    step_string2factor(all_nominal()) %>% 
    step_impute_median(all_numeric()) %>% 
    step_impute_mode(all_nominal(), -lapse) %>% 
    step_zv(all_predictors()) %>% 
    step_dummy(all_nominal(), -lapse)
  
  
  # filter out context features if job uses passive only
  if (feature_set == "passive_only") {
    rec <- rec %>%
      step_rm(contains("context"))
  } 
  
  # control for unbalanced outcome variable
  if (resample == "up") {
    rec <- rec %>% 
      themis::step_upsample(lapse)
  } else if (resample == "down") {
    rec <- rec %>% 
      themis::step_downsample(lapse) 
  } else if (resample == "smote") {
    rec <- rec %>% 
      themis::step_smote(lapse) 
  }
  
  # algorithm specific steps
  if (algorithm == "glmnet" | algorithm == "knn") {
    rec <- rec %>% 
      step_normalize(all_predictors())
  } 
  
  return(rec)
}

tune_model <- function(job, rec, folds) {
  # job: single-row job-specific tibble from jobs
  # folds: rset object that contains all resamples
  # rec: recipe (created manually or via build_recipe() function)
  
  if (job$algorithm == "glmnet") {
    # use whole dataset (all folds)
    # CHANGE: number of penalty values in tune grid
    grid_penalty <- expand_grid(penalty = exp(seq(-5, 5, length.out = 100)))
    
    # tune_grid - takes in recipe, splits, and hyperparameter values to find
    # the best penalty value across all 100 held out folds 
    models <- logistic_reg(penalty = tune(),
                           mixture = job$hp1) %>%
      set_engine("glmnet") %>%
      set_mode("classification") %>%
      tune_grid(preprocessor = rec,
                resamples = folds,
                grid = grid_penalty,
                metrics = metric_set(accuracy, bal_accuracy,
                                     sens, spec, roc_auc))
    
    # create tibble of penalty and metrics returned (avg over 10 folds for each penalty)
    results <- collect_metrics(models) %>% 
      # summarise across repeats
      group_by(penalty, .metric, .estimator, .config) %>% 
      summarise(mean = mean(mean), 
                std_err = mean(std_err), .groups = "drop") %>% 
      select(hp2 = penalty, .metric, mean) %>% 
      pivot_wider(., names_from = ".metric",
                  values_from = "mean") %>% 
      bind_cols(job %>% select(-hp2), .) %>% 
      relocate(hp2, .before = hp3) %>% 
      relocate(sens, .after = accuracy) %>%  # order metrics to bind with other algorithms
      relocate(spec, .after = sens)
    
    return(results)
  }
  
  if (job$algorithm == "random_forest") {
    # extract fold associated with this job - 1 held in and 1 held out set and make 1 
    # set of features for the held in and held out set 
    features <- make_features(job = job, folds = folds, rec = rec)
    feat_in <- features$feat_in
    feat_out <- features$feat_out
    
    # fit model on feat_in with job hyperparemeter values 
    model <- rand_forest(mtry = job$hp1,
                         min_n = job$hp2,
                         trees = job$hp3) %>%
      set_engine("ranger",
                 importance = "impurity",
                 respect.unordered.factors = "order",
                 oob.error = FALSE,
                 seed = 102030) %>%
      set_mode("classification") %>%
      fit(lapse ~ .,
          data = feat_in)
  }
  
  # Single model jobs
  # use get_metrics function to get a tibble that shows performance metrics
  results <- get_metrics(model = model, feat_out = feat_out) %>% 
    pivot_wider(., names_from = "metric",
                values_from = "estimate") %>%   
    bind_cols(job, .) 
  
  return(results) 
}



# helper function for tune_model()
make_features <- function(job, folds, rec) {
  
  # job: single-row job-specific tibble
  # folds: rset object that contains all resamples
  # rec: recipe (created manually or via build_recipe() function)
  
  n_repeats <- as.numeric(str_split(job$cv_type, "_x_")[[1]][1])
  fold_index <- job$n_fold + (job$n_repeat - 1) * n_repeats
  
  d_in <- analysis(folds$splits[[fold_index]])
  d_out <- assessment(folds$splits[[fold_index]])
  
  feat_in <- rec %>% 
    prep(training = d_in) %>% 
    bake(new_data = NULL)
  
  feat_out <- rec %>% 
    prep(training = d_in) %>% 
    bake(new_data = d_out)
  
  return(list(feat_in = feat_in, feat_out = feat_out))
  
}

# helper function for tune_model()
get_metrics <- function(model, feat_out) {
  
  # model: single model object 
  # feat_out: feature matrix built from held-out data
  
  preds <- predict(model, feat_out, type = "class")$.pred_class
  
  cm <- tibble(truth = feat_out$lapse,
               estimate = preds) %>% 
    conf_mat(truth, estimate)
  
  model_metrics <- cm %>% 
    summary(event_level = "second") %>% 
    select(metric = .metric,
           estimate = .estimate) %>% 
    filter(metric %in% c("accuracy", "sens", "spec","bal_accuracy",)) %>% 
    suppressWarnings() # warning not about metrics we are returning
  
  roc <- tibble(truth = feat_out$lapse,
                prob = predict(model, feat_out,
                               type = "prob")$.pred_yes) %>% 
    roc_auc(prob, truth = truth, event_level = "second") %>% 
    select(metric = .metric,
           estimate = .estimate)
  
  model_metrics <- bind_rows(model_metrics, roc)
  
  return(model_metrics)
}






