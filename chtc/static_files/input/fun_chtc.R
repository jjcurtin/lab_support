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
  path_templates <- "../lab_support/chtc/static_files"
  # path_chtc <- "../lab_support/chtc"
  
  # create jobs tibble for K-fold ---------------
  if (cv_type != "boot") {
    
    # Get repeats and folds from cv_type
    cv_repeats <- if (str_split(str_remove(cv_type, "_x"), "_")[[1]][1] == "group") {
      as.numeric(str_split(str_remove(cv_type, "_x"), "_")[[1]][3])
    } else if (str_split(str_remove(cv_type, "_x"), "_")[[1]][1] == "kfold") {
      as.numeric(str_split(str_remove(cv_type, "_x"), "_")[[1]][2])
    }
    
    cv_folds <- if (str_split(str_remove(cv_type, "_x"), "_")[[1]][1] == "group") {
      as.numeric(str_split(str_remove(cv_type, "_x"), "_")[[1]][4])
    } else if (str_split(str_remove(cv_type, "_x"), "_")[[1]][1] == "kfold") {
      as.numeric(str_split(str_remove(cv_type, "_x"), "_")[[1]][3])
    }
    
    
    for (i in algorithm) {
      if (i == "glmnet") { 
        jobs_tmp <- expand_grid(n_repeat = 1:cv_repeats,
                                n_fold = 1:cv_folds,
                                algorithm = "glmnet",
                                feature_set,
                                hp1 = hp1_glmnet,
                                hp2 = NA_integer_,
                                hp3 = NA_integer_,
                                resample)
      } else if (i == "random_forest") {
        jobs_tmp <- expand_grid(n_repeat = 1:cv_repeats,
                                n_fold = 1:cv_folds,
                                algorithm = "random_forest",
                                feature_set,
                                hp1 = hp1_rf,
                                hp2 = hp2_rf,
                                hp3 = hp3_rf,
                                resample)
      } else if (i == "knn") {
        jobs_tmp <- expand_grid(n_repeat = 1:cv_repeats,
                                n_fold = 1:cv_folds,
                                algorithm = "knn",
                                feature_set,
                                hp1 = hp1_knn,
                                hp2 = NA_integer_,
                                hp3 = NA_integer_,
                                resample)      
      } else if (i == "xgboost") {
        jobs_tmp <- expand_grid(n_repeat = 1:cv_repeats,
                                n_fold = 1:cv_folds,
                                algorithm = "xgboost",
                                feature_set,
                                hp1 = hp1_xgboost,
                                hp2 = hp2_xgboost,
                                hp3 = hp3_xgboost,
                                resample)      
      }
      
      # bind jobs files
      jobs <- if (i == algorithm[1])
        jobs_tmp
      else
        rbind(jobs, jobs_tmp)
    }
  }
  
  # modification for bootstrap jobs tibble (no repeats and folds)
  
  if (cv_type == "boot") {
    
    for (i in algorithm) {
      if (i == "glmnet") { 
        jobs_tmp <- expand_grid(n_repeat = NA_integer_,
                                n_fold = NA_integer_,
                                algorithm = "glmnet",
                                feature_set,
                                hp1 = hp1_glmnet,
                                hp2 = NA_integer_,
                                hp3 = NA_integer_,
                                resample)
      } else if (i == "random_forest") {
        jobs_tmp <- expand_grid(n_repeat = NA_integer_,
                                n_fold = NA_integer_,
                                algorithm = "random_forest",
                                feature_set,
                                hp1 = hp1_rf,
                                hp2 = hp2_rf,
                                hp3 = hp3_rf,
                                resample)
      } else if (i == "knn") {
        jobs_tmp <- expand_grid(n_repeat = NA_integer_,
                                n_fold = NA_integer_,
                                algorithm = "knn",
                                feature_set,
                                hp1 = hp1_knn,
                                hp2 = NA_integer_,
                                hp3 = NA_integer_,
                                resample)      
      } else if (i == "xgboost") {
        jobs_tmp <- expand_grid(n_repeat = NA_integer_,
                                n_fold = NA_integer_,
                                algorithm = "xgboost",
                                feature_set,
                                hp1 = hp1_xgboost,
                                hp2 = hp2_xgboost,
                                hp3 = hp3_xgboost,
                                resample)   
      }
      # bind jobs files
      jobs <- if (i == algorithm[1])
        jobs_tmp
      else
        rbind(jobs, jobs_tmp)
    }
    
  }
  
  
  # add job num to file --------------- 
  jobs <- jobs %>% 
    rownames_to_column("job_num") 
  
  # create new job directory (if it does not already exist) -------------------
  if (!dir.exists(file.path(path_jobs, name_job))) {
    dir.create(file.path(path_jobs, name_job))
    dir.create(file.path(path_jobs, name_job, "input"))
    dir.create(file.path(path_jobs, name_job, "output"))
  } else {
    message("Job folder already exists. No new folders created.")
  }
  
  # write jobs file to input folder ---------------
  jobs %>% 
    vroom_write(file.path(path_jobs, name_job, "input", "jobs.csv"), delim = ",")
  
  # write text file of job nums to read into CHTC with submit script (for naming error files)
  jobs %>% 
    select(job_num) %>% 
    write_csv(file.path(path_jobs, name_job, "input", "job_nums.txt"), col_names = FALSE)
    
  
  # copy data to input folder as data_trn -----------------
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
  } else fn <- NULL # set to NULL because you do not want this written out in submit file
  
  # copy study specific training_controls to input folder -----------------
  check_copy <-file.copy(from = file.path(path_training_controls),
            to = file.path(path_jobs, name_job, "input", "training_controls.R"),
            overwrite = overwrite_jobs) 
  if (!check_copy) {
    stop("Training controls not copied to input folder. Check path_training_controls in mak_jobs.")
  }
  
  # copy template R and unix files to input folder -----------------
  check_copy <- file.copy(from = file.path(path_templates, "input", c(list.files(file.path(path_templates, "input")))),
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
    # don't add data_trn to transfer riles if staging
    transfer_files_str <- str_c("transfer_input_files = http://proxy.chtc.wisc.edu/SQUID/chtc/R402.tar.gz, ",
                                paste(tar, collapse = ', '), 
                                ", fun_chtc.R, fit_chtc.R, training_controls.R, jobs.csv, job_nums.txt, http://proxy.chtc.wisc.edu/SQUID/SLIBS.tar.gz", fn)
  } else {
    transfer_files_str <- str_c("transfer_input_files = http://proxy.chtc.wisc.edu/SQUID/chtc/R402.tar.gz, ",
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





make_splits <- function(d, cv_type, group = NULL) {
  
  # d: (training) dataset to be resampled 
  
  # bootstrap splits
  if (cv_type == "boot") {
    # add bootstap splits here
  }
  
  
  # get n_folds and n_repeats if any type of kfold
  if (str_detect(cv_type, "kfold")) {
    n_folds <- cv_type %>% 
      str_extract("_x_\\d{1,2}") %>% 
      str_remove("_x_") %>% 
      as.numeric()
    
    n_repeats <- cv_type %>% 
      str_extract("\\d{1,3}_x_") %>% 
      str_remove("_x_") %>% 
      as.numeric()
  }
  
  # standard kfold
  if (str_detect(cv_type, "^kfold")) {  # starts with kfold
    splits <- d %>% 
      vfold_cv(v = n_folds, repeats = n_repeats) 
  }
    
  # grouped kfold
  if (str_detect(cv_type, "group")) {

    for (i in 1:n_repeats) {
      split <- d %>% 
        group_vfold_cv(group = all_of(group), v = n_folds) # %>% 
        # mutate(id = str_replace(id, "Resample", "Fold"),
        #        id = str_c(str_c("Repeat", str_pad(i, 2, "left", "0")), " ", id))  # assumes repeats < 100
      
      splits <- if (i == 1)
        split
      else
        rbind(splits, split)
    } 
  }
  
  return(splits)
}

make_rset <- function(folds, n_repeat, n_fold, cv_type) {
# used to make an rset object that contains a single split for use in tuning glmnet on CHTC  
  
  n_folds <- cv_type %>% 
    str_extract("_x_\\d{1,2}") %>% 
    str_remove("_x_") %>% 
    as.numeric()
  
  fold_index <- n_fold + (n_repeat - 1) * n_folds
  
  
  fold <- folds[fold_index, ]
  rset <- manual_rset(fold$splits, fold$id)
  return(rset)
}

tune_model <- function(job, rec, folds, cv_type, hp2_glmnet_min = NULL,
                       hp2_glmnet_max = NULL, hp2_glmnet_out = NULL) {
  # job: single-row job-specific tibble from jobs
  # folds: rset object that contains all resamples
  # rec: recipe (created manually or via build_recipe() function)
  
  if (job$algorithm == "glmnet") {
    grid_penalty <- expand_grid(penalty = exp(seq(hp2_glmnet_min, hp2_glmnet_max, length.out = hp2_glmnet_out)))
    
    # make rset for single held-in/held_out split
    split <- make_rset(folds, job$n_repeat, job$n_fold, cv_type)
    
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
    features <- make_features(job = job, folds = folds, rec = rec, cv_type = cv_type)
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
    features <- make_features(job = job, folds = folds, rec = rec, cv_type = cv_type)
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
    features <- make_features(job = job, folds = folds, rec = rec, cv_type = cv_type)
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
make_features <- function(job, folds, rec, cv_type) {
  
  # need to also pass in cv_type if becomes global parameter
  
  # job: single-row job-specific tibble
  # folds: rset object that contains all resamples
  # rec: recipe (created manually or via build_recipe() function)
  
  if (cv_type != "boot") {
    
    n_folds <- cv_type %>% 
      str_extract("_x_\\d{1,2}") %>% 
      str_remove("_x_") %>% 
      as.numeric()
    
    fold_index <- job$n_fold + (job$n_repeat - 1) * n_folds
    
    d_in <- analysis(folds$splits[[fold_index]])
    d_out <- assessment(folds$splits[[fold_index]])
  }
  
  if (cv_type == "boot") {
    
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
                              type = "prob")$.pred_yes) %>% 
    roc_auc(prob, truth = truth, event_level = "first") %>% 
    select(metric = .metric, 
           estimate = .estimate)
  
  model_metrics <- bind_rows(model_metrics, roc)
  
  return(model_metrics)
}

eval_best_model <- function(best_model, rec, folds) {
# evaluates best model configuration using resamples of data contained in folds.
  
  
  # control grid to save predictions
  ctrl <- control_resamples(save_pred = TRUE, event_level = "first",  
                            extract = function (x) extract_fit_parsnip(x) %>% tidy())
  
  if (best_model$algorithm == "glmnet") {
    
    models <- logistic_reg(penalty = best_model$hp2,
                          mixture = best_model$hp1) %>%
      set_engine("glmnet") %>%
      set_mode("classification") %>%
      fit_resamples(preprocessor = rec,
                    resamples = folds,
                    metrics = metric_set(accuracy, bal_accuracy, roc_auc,
                                     sens, yardstick::spec, ppv, npv),
                    control = ctrl)
  }

  
  if (best_model$algorithm == "random_forest") {
    
    # fit model on feat_in with best_model hyperparameter values 
    models <- rand_forest(mtry = best_model$hp1,
                         min_n = best_model$hp2,
                         trees = best_model$hp3) %>%
      set_engine("ranger",
                 importance = "none",
                 respect.unordered.factors = "order",
                 oob.error = FALSE,
                 seed = 102030) %>%
      set_mode("classification") %>%
      fit_resamples(preprocessor = rec,
                    resamples = folds,
                    metrics = metric_set(accuracy, bal_accuracy, roc_auc,
                                     sens, yardstick::spec, ppv, npv),
                    control = ctrl)
  }

  
  if (best_model$algorithm == "xgboost") {
    
    # fit model on feat_in with best_model hyperparameter values 
    models <- boost_tree(learn_rate = best_model$hp1,
                        tree_depth = best_model$hp2,
                        mtry = best_model$hp3,
                        trees = 100,  # set high but use early stopping
                        stop_iter = 10) %>% 
      set_engine("xgboost",
                 validation = 0.2) %>% 
      set_mode("classification") %>%
      fit_resamples(preprocessor = rec,
                    resamples = folds,
                    metrics = metric_set(accuracy, bal_accuracy, roc_auc,
                                         sens, yardstick::spec, ppv, npv),
                    control = ctrl)
  }
  
  
  if (best_model$algorithm == "knn") {
    
    # fit model - best_model provides number of neighbors
    
    models <- nearest_neighbor(neighbors = best_model$hp1) %>% 
      set_engine("kknn") %>% 
      set_mode("classification") %>% 
      fit_resamples(preprocessor = rec,
                    resamples = folds,
                    metrics = metric_set(accuracy, bal_accuracy, roc_auc,
                                     sens, yardstick::spec, ppv, npv),
                    control = ctrl)
  }
    
    results <- collect_metrics(models, summarize = FALSE) %>%
      pivot_wider(., names_from = ".metric",
                  values_from = ".estimate") %>%
      select(-.estimator) %>% 
      bind_cols(best_model %>% select(algorithm, feature_set, hp1, hp2, hp3, resample), .)
    
    
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
