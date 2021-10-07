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
  require(kknn)
  require(vip)
})


# JC general notes.  Need to make assumption about outcome name or else pass it in as string
# KW: functions currently set where outcome variable is y - however it still has assumption that
# y is a binary outcome variable with yes/no labels.


# Can also have a Rmd script that takes all results and selects best model configuration 
# and also displays hyperparameter plots 


# KW: I am thinking this function could be called from within a script in a study-level CHTC 
# folder. All that would need to be done would be defining the parameters and calling this 
# function. The reason I think the script should be in a study folder is that the parameters 
# and paths will always be changing. A template example of this script will be in lab_support. 

# hyperparameters are set to NULL by default so that only the supplied hyperparameters are used
make_jobs <- function(path_training_controls) {
  # read in study specific controls
  source(path_training_controls)
  
  # relative paths should work from any repo project if a local copy of lab_support exists
  path_templates <- "../lab_support/chtc/static_files"
  path_chtc <- "../lab_support/chtc"
  
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
        jobs_tmp <- expand_grid(n_repeat = NA_integer_,
                                n_fold = NA_integer_,
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
    stop("Job folder already exists. No new folders created.")
  }
  
  # write jobs file to input folder ---------------
  jobs %>% 
    write_csv(file.path(path_jobs, name_job, "input", "jobs.csv"))
  
  # copy data to input folder as data_trn -----------------
  file.copy(from = file.path(path_data, data_trn),
            to = file.path(path_jobs, name_job, "input/data_trn.csv")) %>% 
    invisible()
  
  # copy study specific training_controls to input folder -----------------
  file.copy(from = file.path(path_training_controls),
            to = file.path(path_jobs, name_job, "input/training_controls.R")) %>% 
    invisible()
  
  # copy template R files to input folder -----------------
  file.copy(from = file.path(path_templates, "input", c(list.files(file.path(path_templates, "input")))),
            to = file.path(path_jobs, name_job, "input"),
            recursive = TRUE) %>% 
    invisible()
  
  # copy chtc functions to input folder
  file.copy(from = file.path(path_chtc, "fun_chtc.R"),
            to = file.path(path_jobs, name_job, "input"),
            recursive = TRUE) %>% 
    invisible()
  
  # copy template aggregate script to output folder ---------------
  file.copy(from = file.path(path_templates, "post_chtc_processing.Rmd"),
            to = file.path(path_jobs, name_job, "output/post_chtc_processing.Rmd")) %>% 
    invisible()
  
  # update queue on submit file -----------------
  queue <- str_c("queue ", nrow(jobs))
  write(queue, file.path(path_jobs, name_job, "input/sub.sub"), append = TRUE)
}




# JC FUNCTION NOTE: Don't need job because the resampling technique should be the same for
# all jobs.  Do need to specify resampling technique as parameter(s) but likely as a context
# variable in the calling script
# Should support repeated kfold, repeated grouped kfold and bootstrap
# KW: I added functionality for kfold and group_kfold - just needs bootstrap. Also still 
# needs a way to change cv_type to a global parameter instead of getting it from job.

make_splits <- function(d, job) {
  
  # d: (training) dataset to be resampled 
  # job: single job to get cv_type parameter from - may directly pass in cv_type
  # if it becomes global parameter
  
  # bootstrap splits
  if (job$cv_type == "boot") {
    # add bootstap splits here
  }
  
  cv_type <- if (str_split(str_remove(job$cv_type, "_x"), "_")[[1]][1] == "kfold") {
    "kfold"
  } else if (str_split(str_remove(job$cv_type, "_x"), "_")[[1]][1] == "group") {
    "group_kfold"
  }
  
  
  # kfold splits
  if (cv_type == "kfold"){ 
    n_repeats <- as.numeric(str_split(str_remove(job$cv_type, "_x"), "_")[[1]][2])
    n_folds <- as.numeric(str_split(str_remove(job$cv_type, "_x"), "_")[[1]][3])
    
    split <- d %>% 
      vfold_cv(v = n_folds, repeats = n_repeats) 
  }
  
  # grouped kfold splits 
  # grouping variable is hardcoded to be subid
  if (cv_type == "group_kfold"){ 
    n_repeats <- as.numeric(str_split(str_remove(job$cv_type, "_x"), "_")[[1]][3])
    n_folds <- as.numeric(str_split(str_remove(job$cv_type, "_x"), "_")[[1]][4])
    
    for (i in 1:n_repeats) {
      split <- d %>% 
        group_vfold_cv(group = subid, v = n_folds) %>% 
        mutate(n_repeat = i)
      
      splits <- if (i == 1)
        split
      else
        rbind(splits, split)
    }
  }
  
  return(splits)
}




tune_model <- function(job, rec, folds) {
  # job: single-row job-specific tibble from jobs
  # folds: rset object that contains all resamples
  # rec: recipe (created manually or via build_recipe() function)
  
  if (job$algorithm == "glmnet") {
    # use whole dataset (all folds)
    # CHANGE: number of penalty values in tune grid
    grid_penalty <- expand_grid(penalty = exp(seq(-7, 2, length.out = 100)))
    
    # tune_grid - takes in recipe, splits, and hyperparameter values to find
    # the best penalty value across all folds 
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
      summarise(mean = mean(mean), .groups = "drop") %>% 
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
      fit(y ~ .,
          data = feat_in)
    
    # use get_metrics function to get a tibble that shows performance metrics
    results <- get_metrics(model = model, feat_out = feat_out) %>% 
      pivot_wider(., names_from = "metric",
                  values_from = "estimate") %>%   
      bind_cols(job, .) 
    
    return(results) 
  }
  
  if (job$algorithm == "knn") {
    # extract single fold associated with job
    features <- make_features(job = job, folds = folds, rec = rec)
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
      bind_cols(job, .) 
    
    return(results) 
  }
  
}



# helper function for tune_model()
# KW: still need to add section for bootstrap
make_features <- function(job, folds, rec) {
  
  # need to also pass in cv_type if becomes global parameter
  
  # job: single-row job-specific tibble
  # folds: rset object that contains all resamples
  # rec: recipe (created manually or via build_recipe() function)
  
  if (job$cv_type != "boot") {
    
    n_repeats <- if (str_split(str_remove(job$cv_type, "_x"), "_")[[1]][1] == "kfold") {
      as.numeric(str_split(str_remove(job$cv_type, "_x"), "_")[[1]][2])
    } else if (str_split(str_remove(job$cv_type, "_x"), "_")[[1]][1] == "group") {
      as.numeric(str_split(str_remove(job$cv_type, "_x"), "_")[[1]][3])
    }
    
    fold_index <- job$n_fold + (job$n_repeat - 1) * n_repeats
    
    d_in <- analysis(folds$splits[[fold_index]])
    d_out <- assessment(folds$splits[[fold_index]])
  }
  
  if (job$cv_type == "boot") {
    
    # pull out bootstrap split here
    
  }
  
  # make feature matrices
  feat_in <- rec %>% 
    prep(training = d_in) %>% 
    bake(new_data = NULL)
  
  feat_out <- rec %>% 
    prep(training = d_in) %>% 
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
    summary(event_level = "second") %>% 
    select(metric = .metric,
           estimate = .estimate) %>% 
    filter(metric %in% c("accuracy", "sens", "spec", "bal_accuracy")) %>% 
    suppressWarnings() # warning not about metrics we are returning
  
  roc <- tibble(truth = feat_out$y,
                prob = predict(model, feat_out,
                               type = "prob")$.pred_yes) %>% 
    roc_auc(prob, truth = truth, event_level = "second") %>% 
    select(metric = .metric,
           estimate = .estimate)
  
  model_metrics <- bind_rows(model_metrics, roc)
  
  return(model_metrics)
}






