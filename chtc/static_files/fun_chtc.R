suppressWarnings(suppressPackageStartupMessages({
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
  require(kknn)
  require(vip)
  require(vroom)
}))


# JC general notes.  Need to make assumption about outcome name or else pass it in as string
# KW: functions currently set where outcome variable is y - in training_controls there is a 
# variable for the outcome name in the dataset. In the recipe this is changed to y.   


# Can also have a Rmd script that takes all results and selects best model configuration 
# and also displays hyperparameter plots 
# KW: currently called mak_training_metrics in scripts_parameterized 

 

# Only need to supply hyperparameters in training_controls.R for algorithms being used 
make_jobs <- function(path_training_controls, overwrite_jobs = TRUE) {
  # read in study specific controls
  source(path_training_controls)
  
  # relative paths should work from any repo project if a local copy of lab_support exists

  path_chtc <- "../lab_support/chtc"
  

  # Get split indices from cv resample parameters
  if (cv_resample_type == "boot") {
    split_num <- 1:cv_resample 
    # set nested split_num parameters to NA
    outer_split_num <- NA
    inner_split_num <- NA
  }
  
  if (cv_resample_type == "kfold") {
    n_repeats <- as.numeric(str_remove(cv_resample, "_x_\\d{1,2}"))
    n_folds <- as.numeric(str_remove(cv_resample, "\\d{1,3}_x_"))
    
    split_num <- 1:(n_repeats * n_folds)
    
    # set nested split_num parameters to NA
    outer_split_num <- NA
    inner_split_num <- NA
  }
  
  if (cv_resample_type == "nested") {
    # set split_num to NA and use outer_split_num and inner_split_num
    split_num <- NA
    
    # outer cv loop - always will be kfold
    outer_n_repeats <- as.numeric(str_remove(cv_outer_resample, "_x_\\d{1,2}"))
    outer_n_folds <- as.numeric(str_remove(cv_outer_resample, "\\d{1,3}_x_"))
    
    outer_split_num <- 1:(outer_n_repeats * outer_n_folds)
    
    # inner cv loop - can be kfold or bootstrap
    if (str_detect(cv_inner_resample, "_x_")) {
      inner_n_repeats <- as.numeric(str_remove(cv_inner_resample, "_x_\\d{1,2}"))
      inner_n_folds <- as.numeric(str_remove(cv_inner_resample, "\\d{1,3}_x_"))
      
      inner_split_num <- 1:(inner_n_repeats * inner_n_folds)
    } 
    
    if (!str_detect(cv_inner_resample, "_x_")) {
      inner_split_num <- 1:cv_inner_resample
    }
    
  }
  
  
  # create jobs tibble
  # NOTE: I removed looping through multiple algorithms in a single jobs file on 9/8/22
  if (algorithm == "glmnet") { 
      jobs <- expand_grid(split_num = split_num,
                          outer_split_num = outer_split_num,
                          inner_split_num = inner_split_num,
                          algorithm = algorithm,
                          feature_set,
                          hp1 = hp1_glmnet,
                          hp2 = NA_integer_, # hp2 will be tuned in fit script and written over in results csv
                          hp3 = NA_integer_,
                          resample)
  } 
  
  if (algorithm == "random_forest") {
      jobs <- expand_grid(split_num = split_num,
                          outer_split_num = outer_split_num,
                          inner_split_num = inner_split_num,
                          algorithm = algorithm,
                          feature_set,
                          hp1 = hp1_rf,
                          hp2 = hp2_rf,
                          hp3 = hp3_rf,
                          resample)
  } 
  
  if (algorithm == "knn") {
      jobs <- expand_grid(split_num = split_num,
                          outer_split_num = outer_split_num,
                          inner_split_num = inner_split_num,
                          algorithm = algorithm,
                          feature_set,
                          hp1 = hp1_knn,
                          hp2 = NA_integer_,
                          hp3 = NA_integer_,
                          resample)      
  }  
  
  if (algorithm == "xgboost") {
      jobs <- expand_grid(split_num = split_num,
                          outer_split_num = outer_split_num,
                          inner_split_num = inner_split_num,
                          algorithm = algorithm,
                          feature_set,
                          hp1 = hp1_xgboost,
                          hp2 = hp2_xgboost,
                          hp3 = hp3_xgboost,
                          resample)      
  }
  
  
  # add job num to tibble
  jobs <- jobs %>% 
    tibble::rownames_to_column("job_num") 
  
  # create new job directory (if it does not already exist) 
  if (!dir.exists(file.path(path_jobs, name_job))) {
    dir.create(file.path(path_jobs, name_job))
    dir.create(file.path(path_jobs, name_job, "input"))
    dir.create(file.path(path_jobs, name_job, "output"))
  } else {
    message("Job folder already exists. No new folders created.")
  }

  # write jobs file to input folder
  jobs %>% 
    vroom_write(file.path(path_jobs, name_job, "input", "jobs.csv"), delim = ",")
  
  # write text file of job nums to read into CHTC with submit script (for naming error files)
  jobs %>% 
    select(job_num) %>% 
    write_csv(file.path(path_jobs, name_job, "input", "job_nums.txt"), col_names = FALSE)
    
  

  # copy data to input folder as data_trn 
  # will not copy over large data files to be used with staging (data_trn = NULL in training controls)
  if(!is.null(data_trn)){
    chunks <- str_split_fixed(data_trn, "\\.", n = Inf) # parse name from extensions
    if (length(chunks) == 2) {
      fn <- str_c("data_trn.", chunks[[2]])
    } else {
      fn <- str_c("data_trn.", chunks[[2]], ".", chunks[[3]])
    }
    check_copy <- file.copy(from = file.path(path_data, data_trn),
                            to = file.path(path_jobs, name_job, "input", fn),
                            overwrite = overwrite_jobs)
    if (!check_copy) {
      stop("data_trn not copied to input folder. Check path_data and data_trn (file name) in training controls.")
    }
  } else fn <- NULL # set to NULL because you do not want this written out in submit file (for chtc staging)
  
  # copy study specific training_controls to input folder 
  check_copy <-file.copy(from = file.path(path_training_controls),
            to = file.path(path_jobs, name_job, "input", "training_controls.R"),
            overwrite = overwrite_jobs) 
  if (!check_copy) {
    stop("Training controls not copied to input folder. Check path_training_controls in mak_jobs.")
  }
  
  # copy static R and unix chtc files to input folder 
  check_copy <- file.copy(from = file.path(path_chtc, "static_files", c(list.files(file.path(path_chtc, "static_files")))),
            to = file.path(path_jobs, name_job, "input"),
            recursive = TRUE,
            overwrite = overwrite_jobs) 
  for (i in 1:length(check_copy)) {
    if (check_copy[i] == FALSE) {
    stop("Not all static files copied to input folder. Make sure you are running mak_jobs in an R project.")
    }
  }
  
  # update submit file from training controls -----------------
  # add files to transfer
  if(is.null(data_trn)) {
    # don't add data_trn to transfer files if staging
    transfer_files_str <- str_c("transfer_input_files = http://proxy.chtc.wisc.edu/SQUID/chtc/el8/R413.tar.gz, ",
                                paste(tar, collapse = ', '), 
                                ", fun_chtc.R, fit_chtc.R, training_controls.R, jobs.csv, job_nums.txt, http://proxy.chtc.wisc.edu/SQUID/SLIBS.tar.gz", fn)
  } else {
    transfer_files_str <- str_c("transfer_input_files = http://proxy.chtc.wisc.edu/SQUID/chtc/el8/R413.tar.gz, ",
                                paste(tar, collapse = ', '), ", ", fn,
                                ", fun_chtc.R, fit_chtc.R, training_controls.R, jobs.csv, job_nums.txt, http://proxy.chtc.wisc.edu/SQUID/SLIBS.tar.gz")
  }
    
  write(transfer_files_str, file.path(path_jobs, name_job, "input", "sub.sub"), append = TRUE)
  
  # add requirement for loading glmnet/xgboost
  # also add staging requirement if data_trn is null
  if(is.null(data_trn)) {
    staging_req <- str_c("Requirements = (Target.HasCHTCStaging == true) && (HasChtcSoftware == true)")
    write(staging_req, file.path(path_jobs, name_job, "input", "sub.sub"), append = TRUE)
  } else {
    staging_req <- str_c("Requirements = (HasChtcSoftware == true)")
    write(staging_req, file.path(path_jobs, name_job, "input", "sub.sub"), append = TRUE)
  }
  
  # add max idle jobs
  max_idle_str <- str_c("materialize_max_idle = ", max_idle)
  write(max_idle_str, file.path(path_jobs, name_job, "input", "sub.sub"), append = TRUE)
  
  # add cpus requested
  cpus_str <- str_c("request_cpus = ", request_cpus)
  write(cpus_str, file.path(path_jobs, name_job, "input", "sub.sub"), append = TRUE)
  
  # add memory requested
  memory_str <- str_c("request_memory = ", request_memory)
  write(memory_str, file.path(path_jobs, name_job, "input", "sub.sub"), append = TRUE)
  
  # add disk space requested
  disk_str <- str_c("request_disk = ", request_disk)
  write(disk_str, file.path(path_jobs, name_job, "input", "sub.sub"), append = TRUE)
  
  # add flock
  flock_str <- str_c("+wantFlocking = ", flock)
  write(flock_str, file.path(path_jobs, name_job, "input", "sub.sub"), append = TRUE)
  
  # add glide
  glide_str <- str_c("+wantGlideIn = ", glide)
  write(glide_str, file.path(path_jobs, name_job, "input", "sub.sub"), append = TRUE)
  
  # add queue
  queue_str <- str_c("queue job_num from job_nums.txt")
  write(queue_str, file.path(path_jobs, name_job, "input", "sub.sub"), append = TRUE)
  
}



make_splits <- function(d, cv_resample_type, cv_resample = NULL, cv_outer_resample = NULL, cv_inner_resample = NULL, cv_group = NULL) {
  
  # d: (training) dataset to be resampled 
  # cv_resample_type: can be boot, kfold, or nested
  # resample: specifies for repeats and folds for CV (1_x_10; 10_x_10) or num splits for bootstrapping (100)
  # inner_resample: specifies repeats/folds or num bootstrap splits for nested cv inner loop - same format as above
  # outer_resample: specifies repeats/folds for outer nested cv loop - cannot use bootstrapping here
  # group: specifies grouping variable for grouped cv and nested cv
  
  
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
        group_vfold_cv(v = inner_n_folds, repeats = inner_n_repeats, group = all_of(cv_group))
      splits <- d %>% 
        nested_cv(outside = outer_grouped_kfold, 
                  inside = group_vfold_cv(v = inner_n_folds, repeats = inner_n_repeats, group = all_of(cv_group)))
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

make_rset <- function(splits, cv_resample_type, split_num = NULL, inner_split_num = NULL, outer_split_num = NULL) {
# used to make an rset object that contains a single split for use in tuning glmnet on CHTC  
  
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

tune_model <- function(job, rec, splits, cv_resample_type, hp2_glmnet_min = NULL,
                       hp2_glmnet_max = NULL, hp2_glmnet_out = NULL) {
  # job: single-row job-specific tibble from jobs
  # splits: rset object that contains all resamples
  # rec: recipe (created manually or via build_recipe() function)
  
  if (job$algorithm == "glmnet") {
    grid_penalty <- expand_grid(penalty = exp(seq(hp2_glmnet_min, hp2_glmnet_max, length.out = hp2_glmnet_out)))
    
    # make rset for single held-in/held_out split
    # does not work for bootstrapping
    split <- make_rset(splits, cv_resample_type = cv_resample_type, split_num = job$split_num,
                       inner_split_num = job$inner_split_num, outer_split_num = job$outer_split_num)
    
    models <- logistic_reg(penalty = tune(),
                           mixture = job$hp1) %>%
      set_engine("glmnet") %>%
      set_mode("classification") %>%
      tune_grid(preprocessor = rec,
                resamples = split,
                grid = grid_penalty,
                # metrics assume that positive event it first level
                # make sure this is true in recipe
                metrics = metric_set(accuracy, bal_accuracy, roc_auc,
                                     sens, yardstick::spec, ppv, npv))
    
    # create tibble of penalty and metrics returned 
    results <- collect_metrics(models, summarize = FALSE) %>% 
      rename(hp2 = penalty) %>% 
      select(hp2, .metric, .estimate) %>% # use select to drop extra cols (.estimator, n, std_err, .config)
      pivot_wider(., names_from = ".metric",
                  values_from = ".estimate") %>%  
      relocate(sens, spec, ppv, npv, accuracy, bal_accuracy, roc_auc) %>% # specify column order of metrics
      bind_cols(job %>% select(-hp2), .) %>% 
      relocate(hp2, .before = hp3) 
    
    return(results)
  }
  
  if (job$algorithm == "random_forest") {
    # extract fold associated with this job - 1 held in and 1 held out set and make 1 
    # set of features for the held in and held out set 
    features <- make_features(job = job, splits = splits, rec = rec, cv_resample_type = cv_resample_type)
    feat_in <- features$feat_in
    feat_out <- features$feat_out
    
    # fit model on feat_in with job hyperparameter values 
    model <- rand_forest(mtry = job$hp1,
                         min_n = job$hp2,
                         trees = job$hp3) %>%
      set_engine("ranger",
                 importance = "none",
                 respect.unordered.factors = "order",
                 oob.error = FALSE,
                 seed = 102030) %>%
      set_mode("classification") %>%
      fit(y ~ .,
          data = feat_in)
    
    # use get_metrics function to get a tibble that shows performance metrics
    results <- get_metrics(model = model, feat_out = feat_out) %>% 
      pivot_wider(., names_from = "metric",
                  values_from = "estimate") %>%   
      relocate(sens, spec, ppv, npv, accuracy, bal_accuracy, roc_auc) %>% 
      bind_cols(job, .) 
    
    return(results)
  }
  
  if (job$algorithm == "xgboost") {
    
    # extract fold associated with this job - 1 held in and 1 held out set and make 1 
    # set of features for the held in and held out set 
    features <- make_features(job = job, splits = splits, rec = rec, cv_resample_type = cv_resample_type)
    feat_in <- features$feat_in
    feat_out <- features$feat_out
    
    # fit model on feat_in with job hyperparameter values 
    model <- boost_tree(learn_rate = job$hp1,
                        tree_depth = job$hp2,
                        mtry = job$hp3,
                        trees = 100,  # set high but use early stopping
                        stop_iter = 10) %>% 
      set_engine("xgboost",
                 validation = 0.2) %>% 
      set_mode("classification") %>%
      fit(y ~ ., data = feat_in)
    
    # use get_metrics function to get a tibble that shows performance metrics
    results <- get_metrics(model = model, feat_out = feat_out) %>% 
      pivot_wider(., names_from = "metric",
                  values_from = "estimate") %>%   
      relocate(sens, spec, ppv, npv, accuracy, bal_accuracy, roc_auc) %>% 
      bind_cols(job, .) 
    
    return(results)
  }
  
  if (job$algorithm == "knn") {
    # extract single fold associated with job
    features <- make_features(job = job, splits = splits, rec = rec, cv_resample_type = cv_resample_type)
    feat_in <- features$feat_in
    feat_out <- features$feat_out
    
    # fit model - job provides number of neighbors
    model <- nearest_neighbor(neighbors = job$hp1) %>% 
      set_engine("kknn") %>% 
      set_mode("classification") %>% 
      fit(y ~ .,
          data = feat_in)
    
    # use get_metrics function to get a tibble that shows performance metrics
    results <- get_metrics(model = model, feat_out = feat_out) %>% 
      pivot_wider(., names_from = "metric",
                  values_from = "estimate") %>%   
      relocate(sens, spec, ppv, npv, accuracy, bal_accuracy, roc_auc) %>% 
      bind_cols(job, .) 
    
    return(results) 
  }
  
}



# helper function for tune_model()
# KW: still need to add section for bootstrap
make_features <- function(job, splits, rec, cv_resample_type) {
  

  # job: single-row job-specific tibble
  # splits: rset object that contains all resamples
  # rec: recipe (created manually or via build_recipe() function)
  # cv_resample_type: either boot, kfold, or nested
  
  if (cv_resample_type == "kfold") {
    
    d_in <- training(splits$splits[[job$split_num]])
    d_out <- testing(splits$splits[[job$split_num]])
  }
  
  
  if (cv_resample_type == "nested") {
    
    d_in <- training(splits$inner_resamples[[job$outer_split_num]]$splits[[job$inner_split_num]])

    d_out <- testing(splits$inner_resamples[[job$outer_split_num]]$splits[[job$inner_split_num]]) 
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
# KW: note this only works on binary yes, no outcome (y)
get_metrics <- function(model, feat_out) {
  
  # model: single model object 
  # feat_out: feature matrix built from held-out data
  
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
                              type = "prob")$.pred_pos) %>% 
    roc_auc(prob, truth = truth, event_level = "first") %>% 
    select(metric = .metric, 
           estimate = .estimate)
  
  model_metrics <- bind_rows(model_metrics, roc)
  
  return(model_metrics)
}

eval_best_model <- function(config_best, rec, splits) {
# evaluates best model configuration using resamples of data contained in splits.
  
  
  # control grid to save predictions
  ctrl <- control_resamples(save_pred = TRUE, event_level = "second",  
                            extract = function (x) extract_fit_parsnip(x) %>% tidy())
  
  if (config_best$algorithm == "glmnet") {
    
    models <- logistic_reg(penalty = config_best$hp2,
                          mixture = config_best$hp1) %>%
      set_engine("glmnet") %>%
      set_mode("classification") %>%
      fit_resamples(preprocessor = rec,
                    resamples = splits,
                    metrics = metric_set(accuracy, bal_accuracy, roc_auc,
                                     sens, yardstick::spec, ppv, npv),
                    control = ctrl)
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
      set_mode("classification") %>%
      fit_resamples(preprocessor = rec,
                    resamples = splits,
                    metrics = metric_set(accuracy, bal_accuracy, roc_auc,
                                     sens, yardstick::spec, ppv, npv),
                    control = ctrl)
  }

  
  if (config_best$algorithm == "xgboost") {
    
    # fit model on feat_in with config_best hyperparameter values 
    models <- boost_tree(learn_rate = config_best$hp1,
                        tree_depth = config_best$hp2,
                        mtry = config_best$hp3,
                        trees = 100,  # set high but use early stopping
                        stop_iter = 10) %>% 
      set_engine("xgboost",
                 validation = 0.2) %>% 
      set_mode("classification") %>%
      fit_resamples(preprocessor = rec,
                    resamples = splits,
                    metrics = metric_set(accuracy, bal_accuracy, roc_auc,
                                         sens, yardstick::spec, ppv, npv),
                    control = ctrl)
  }
  
  
  if (config_best$algorithm == "knn") {
    
    # fit model - config_best provides number of neighbors
    
    models <- nearest_neighbor(neighbors = config_best$hp1) %>% 
      set_engine("kknn") %>% 
      set_mode("classification") %>% 
      fit_resamples(preprocessor = rec,
                    resamples = splits,
                    metrics = metric_set(accuracy, bal_accuracy, roc_auc,
                                     sens, yardstick::spec, ppv, npv),
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

fit_best_model <- function(best_model, rec, d) {
  
  # make features for full dataset
  feat <- rec %>% 
    prep(training = d, strings_as_factors = FALSE) %>% 
    bake(new_data = NULL)
  
  if (best_model$algorithm == "glmnet") {
    
    fit_best <- logistic_reg(penalty = best_model$hp2,
                           mixture = best_model$hp1) %>%
      set_engine("glmnet") %>%
      set_mode("classification") %>%
      fit(y ~ ., data = feat)
    
    return(fit_best)
  }
  
  if (best_model$algorithm == "random_forest") {
    
    fit_best <- rand_forest(mtry = best_model$hp1,
                          min_n = best_model$hp2,
                          trees = best_model$hp3) %>%
      set_engine("ranger",
                 importance = "impurity_corrected",
                 respect.unordered.factors = "order",
                 oob.error = FALSE,
                 seed = 102030) %>%
      set_mode("classification") %>%
      fit(y ~ ., data = feat)
    
    
    return(fit_best)
  }
  
  if (best_model$algorithm == "xgboost") {
    
    fit_best <- boost_tree(learn_rate = best_model$hp1,
                           tree_depth = best_model$hp2,
                           mtry = best_model$hp3,
                           trees = 100,  # set high but use early stopping
                           stop_iter = 10) %>% 
      set_engine("xgboost",
                 validation = 0.2) %>% 
      set_mode("classification") %>%
      fit(y ~ ., data = feat)
    
    return(fit_best)
  }
  
  if (best_model$algorithm == "knn") {
    
    fit_best <- nearest_neighbor(neighbors = best_model$hp1) %>% 
      set_engine("kknn") %>% 
      set_mode("classification") %>% 
      fit(y ~ ., data = feat)
    
    return(fit_best)
  }
}
