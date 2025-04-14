make_jobs <- function(path_training_controls, overwrite_batch = TRUE) {
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
  
  
  # create configs tibble
  if (algorithm == "glmnet") { 
    configs <- expand_grid(split_num = split_num,
                           outer_split_num = outer_split_num,
                           inner_split_num = inner_split_num,
                           algorithm = algorithm,
                           feature_set,
                           hp1 = hp1_glmnet,
                           # hp2 will be tuned in fit script and written over in results csv
                           hp2 = NA_integer_, 
                           hp3 = NA_integer_,
                           resample)
  } 
  
  if (algorithm == "random_forest") {
    configs <- expand_grid(split_num = split_num,
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
    configs <- expand_grid(split_num = split_num,
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
    configs <- expand_grid(split_num = split_num,
                           outer_split_num = outer_split_num,
                           inner_split_num = inner_split_num,
                           algorithm = algorithm,
                           feature_set,
                           hp1 = hp1_xgboost,
                           hp2 = hp2_xgboost,
                           hp3 = hp3_xgboost,
                           resample)      
  }
  
  if (algorithm == "glm") { 
    configs <- expand_grid(split_num = split_num,
                           outer_split_num = outer_split_num,
                           inner_split_num = inner_split_num,
                           algorithm = algorithm,
                           feature_set,
                           hp1 = NA,
                           hp2 = NA,
                           hp3 = NA,
                           resample)
  } 
  
  if (algorithm == "glmnet_manual") { 
    configs <- expand_grid(split_num = split_num,
                           outer_split_num = outer_split_num,
                           inner_split_num = inner_split_num,
                           algorithm = algorithm,
                           feature_set,
                           hp1 = hp1_glmnet_manual,
                           hp2 = hp2_glmnet_manual, 
                           hp3 = NA_integer_,
                           resample)
  } 
  
  if (algorithm == "rda") {
    configs <- expand_grid(split_num = split_num,
                           outer_split_num = outer_split_num,
                           inner_split_num = inner_split_num,
                           algorithm = algorithm,
                           feature_set,
                           hp1 = hp1_rda,
                           hp2 = hp2_rda,
                           hp3 = NA_integer_,
                           resample)      
  }  
  
  if (algorithm == "nnet") {
    configs <- expand_grid(split_num = split_num,
                           outer_split_num = outer_split_num,
                           inner_split_num = inner_split_num,
                           algorithm = algorithm,
                           feature_set,
                           hp1 = hp1_nnet,
                           hp2 = hp2_nnet,
                           hp3 = hp3_nnet,
                           resample)      
  }  
  
  
  # add config num to configs
  configs <- configs %>% 
    tibble::rownames_to_column("config_num") |> 
    mutate(config_num = as.numeric(config_num))
  
  # create new batch directory (if it does not already exist) 
  if (!dir.exists(file.path(path_batch))) {
    dir.create(file.path(path_batch))
    dir.create(file.path(path_batch, "input"))
    dir.create(file.path(path_batch, "output"))
  } else {
    stop("Batch folder already exists. No new folders created. Set overwrite_batch = TRUE to write over existing batch.")
  }
  
  # write jobs file to input folder
  configs %>% 
    write_csv(file.path(path_batch, "input", "configs.csv"))
  
  # write text file of config_start and config_end for each CHTC job
  seq_end <- max(configs$config_num)
  
  config_start <- seq(from = 1, to = seq_end, by = configs_per_job)
  config_end <- seq(from = configs_per_job, to = seq_end, by = configs_per_job)
  
  # add one more end if end_seq not a multiple of configs_per_job
  if (length(config_end) < length(config_start)) config_end <- c(config_end, seq_end)
  
  
  # create job_nums.csv file  
  tibble(config_start, config_end) |> 
    tibble::rownames_to_column("job_num") |> 
    mutate(job_num = as.numeric(job_num)) |> 
    write_csv(file.path(path_batch, "input", "job_nums.csv"), 
              col_names = FALSE)
  
  # copy data to input folder as data_trn 
  # will not copy over large data files to be used with staging (stage_data = TRUE in training controls)
  if(stage_data == FALSE){
    chunks <- str_split_fixed(data_trn, "\\.", n = Inf) # parse name from extensions
    if (length(chunks) == 2) {
      fn <- str_c("data_trn.", chunks[[2]])
    } else {
      fn <- str_c("data_trn.", chunks[[2]], ".", chunks[[3]])
    }
    check_copy <- file.copy(from = file.path(path_data, data_trn),
                            to = file.path(path_batch, "input", fn),
                            overwrite = overwrite_batch)
    if (!check_copy) {
      stop("data_trn not copied to input folder. Check path_data and data_trn (file name) in training controls.")
    }
  } else {
    chunks <- str_split_fixed(data_trn, "\\.", n = Inf) # parse name from extensions
    if (length(chunks) == 2) {
      fn <- str_c("data_trn.", chunks[[2]])
    } else {
      fn <- str_c("data_trn.", chunks[[2]], ".", chunks[[3]])
    }
  }
  
  # copy study specific training_controls to input folder 
  check_copy <- file.copy(from = file.path(path_training_controls),
                          to = file.path(path_batch, "input", "training_controls.R"),
                          overwrite = overwrite_batch) 
  if (!check_copy) {
    stop("Training controls not copied to input folder. Check path_training_controls in mak_jobs.")
  }
  
  # copy static R and unix chtc files to input folder 
  check_copy <- file.copy(from = file.path(path_chtc, "static_files", 
                                           c(list.files(file.path(path_chtc, "static_files")))),
                          to = file.path(path_batch, "input"),
                          recursive = TRUE,
                          overwrite = overwrite_batch) 
  for (i in 1:length(check_copy)) {
    if (check_copy[i] == FALSE) {
      stop("Not all static files copied to input folder. Make sure you are running mak_jobs in an R project.")
    }
  }
  
  # update submit file from training controls -----------------
  # add files to transfer
  if(stage_data == FALSE) {
    transfer_files_str <- str_c("transfer_input_files = fun_chtc.R, fit_chtc.R, training_controls.R, configs.csv, job_nums.csv, osdf:///chtc/staging/", username, "/train.sif,", fn)
  } else {
    transfer_files_str <- str_c("transfer_input_files = fun_chtc.R, fit_chtc.R, training_controls.R, configs.csv, job_nums.csv, osdf:///chtc/staging/", username, "/train.sif, osdf:///chtc/staging/", username, "/", fn)
  }
  
  write(transfer_files_str, file.path(path_batch, "input", "train.sub"), append = TRUE)
  
  # add max idle jobs
  max_idle_str <- str_c("max_idle = ", max_idle)
  write(max_idle_str, file.path(path_batch, "input", "train.sub"), append = TRUE)
  
  # add cpus requested
  cpus_str <- str_c("request_cpus = ", request_cpus)
  write(cpus_str, file.path(path_batch, "input", "train.sub"), append = TRUE)
  
  # add memory requested
  memory_str <- str_c("request_memory = ", request_memory)
  write(memory_str, file.path(path_batch, "input", "train.sub"), append = TRUE)
  
  # add disk space requested
  disk_str <- str_c("request_disk = ", request_disk)
  write(disk_str, file.path(path_batch, "input", "train.sub"), append = TRUE)
  
  # add flock
  flock_str <- str_c("want_campus_pools = ", flock)
  write(flock_str, file.path(path_batch, "input", "train.sub"), append = TRUE)
  
  # add glide
  glide_str <- str_c("want_ospool = ", glide)
  write(glide_str, file.path(path_batch, "input", "train.sub"), append = TRUE)
  
  # add queue
  queue_str <- str_c("queue job_num,config_start,config_end from job_nums.csv")
  write(queue_str, file.path(path_batch, "input", "train.sub"), append = TRUE)
}