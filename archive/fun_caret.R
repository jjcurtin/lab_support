#NOTES:  This script contains functions that will eventually be added to a custom library for IAML
library(rsample)
library(tidyverse)
library(boot)
library(OptimalCutpoints)


#Function: simplify_cv---------------------------------
#Returns a simpler, clearer, and more consistent (across singleloop and nested cv) list structure 
#for use in CV with caret or HTC
simplify_cv <- function(iters) {
  library(stringr)
  
  
  resamples <- list()
  n <- nrow(iters$splits[[1]]$data)
  
  #determine if nested CV
  if ("inner_resamples" %in% names(iters)) {   
    #nested cv
    resamples$cv_type <- 'nested'
    
    #number of outer folds/repeats 
    if ("id2" %in% names(iters)) {
      n_folds_outer <- max(as.numeric(str_extract(iters$id, "(\\d)+$")))
      n_repeats_outer <- max(as.numeric(str_extract(iters$id, "(\\d)+$")))
    }else{
      n_folds_outer <- max(as.numeric(str_extract(iters$id, "(\\d)+$")))
      n_repeats_outer <- 1
    }
    resamples$n_folds_outer <- n_folds_outer
    resamples$n_repeats_outer <- n_repeats_outer
    
    #number of inner folds/repeats
    if ("id2" %in% names(iters$inner_resamples[[1]])) {
      n_folds_inner <- max(as.numeric(str_extract(iters$inner_resamples[[1]]$id2, "(\\d)+$")))
      n_repeats_inner <- max(as.numeric(str_extract(iters$inner_resamples[[1]]$id, "(\\d)+$")))
    }else{
      n_folds_inner <- max(as.numeric(str_extract(iters$inner_resamples[[1]]$id, "(\\d)+$")))
      n_repeats_inner <- 1
    }
    resamples$n_folds_inner <- n_folds_inner
    resamples$n_repeats_inner <- n_repeats_inner
    
    for (i in 1:length(iters$splits)) {
      resamples$outer_in_id[i] <- list(iters$splits[[i]]$in_id)
      resamples$outer_out_id[i] <- list(setdiff(1:n, iters$splits[[i]]$in_id))
    }
    
    if ("id2" %in% names(iters)) {
      outer_names <- str_to_lower(str_c("outer_", iters$id2, "_", iters$id))
    } else {
      outer_names <- str_to_lower(str_c("outer_", iters$id))
    }
    names(resamples$outer_in_id) <- outer_names
    names(resamples$outer_out_id) <- outer_names
    
    #need to take real IDs/rows across inner and outer
    all_rows <- 1:n
    for (i in 1:length(iters$inner_resamples)) { 
      inner_in_id <- list()
      inner_out_id <- list()
      in_rows <- all_rows[iters$splits[[i]]$in_id]
      
      for (j in 1:length(iters$inner_resamples[[i]]$splits)) {
        inner_in_id[j] <- list(in_rows[iters$inner_resamples[[i]]$splits[[j]]$in_id])
        inner_out_id[j] <- list(setdiff(in_rows, inner_in_id[[j]]))
      }
      
      if ("id2" %in% names(iters$inner_resamples[[i]])) {
        inner_names = str_to_lower(str_c("inner_", iters$inner_resamples[[i]]$id2, "_", iters$inner_resamples[[i]]$id))
        
      } else {
        inner_names <- str_to_lower(str_c("inner_", iters$inner_resamples[[i]]$id, "_"))  
      }
      names(inner_in_id) <- inner_names
      names(inner_out_id) <- inner_names
      
      resamples$inner_in_id[i] <- list(inner_in_id)
      resamples$inner_out_id[i] <- list(inner_out_id)
    }
    if ("id2" %in% names(iters)) {
      names(resamples$inner_in_id) <- str_to_lower(str_c("outer_", iters$id2, "_", iters$id))
    } else {
      names(resamples$inner_in_id) <- str_to_lower(str_c("outer_", iters$id))
    }
    
  } else {
    #single loop CV
    resamples$cv_type <- 'single_loop'
    
    #number of folds/repeats 
    if ("id2" %in% names(iters)) {
      n_folds_inner <- max(as.numeric(str_extract(iters$id, "(\\d)+$")))
      n_repeats_inner <- max(as.numeric(str_extract(iters$id, "(\\d)+$")))
    }else{
      n_folds_inner <- max(as.numeric(str_extract(iters$id, "(\\d)+$")))
      n_repeats_inner <- 1
    }
    resamples$n_folds_inner <- n_folds_inner
    resamples$n_repeats_inner <- n_repeats_inner
    
    
    for (i in 1:length(iters$splits)) {
      resamples$inner_in_id[i] <- list(iters$splits[[i]]$in_id)
      resamples$inner_out_id[i] <- list(setdiff(1:n, iters$splits[[i]]$in_id))
    }
    
    if ("id2" %in% names(iters)) {   
      #repeated CV
      id_names <- str_c(iters$id, "_", iters$id2)
    } else {
      #one CV loop
      id_names <- str_c(iters$id)
    }
    id_names <- str_to_lower(id_names)
    names(resamples$inner_in_id) <- id_names
    names(resamples$inner_out_id) <- id_names
  }
  
  return(resamples)
}

#FUNCTION: mak_iters(d, n_folds_outer, n_repeats_outer, n_folds_inner, n_repeats_inner, seed)-------------------
mak_iters <- function(d, n_folds_outer = 0, n_repeats_outer = 0, n_folds_inner, n_repeats_inner, seed, grouped = FALSE, group_id = NULL){
  
  set.seed(seed)
  
  if (grouped) {
    if (n_folds_outer > 0 | n_repeats_inner > 1) {
      stop("mak_iters does not currently support nested CV or repeated single loop CV")
    }
    
    iters <- group_vfold_cv(d, group = group_id, v = n_folds_inner)
  }else {
    
    if (n_folds_outer == 0) {
      iters <- vfold_cv(d, v = n_folds_inner, repeats = n_repeats_inner, strata = "y")
    }
    if (n_folds_outer > 0) {
      iters <- nested_cv(
        d, 
        outside = vfold_cv(v = n_folds_outer, repeats = n_repeats_outer, strata = "y"), 
        inside = vfold_cv(v = n_folds_inner, repeats = n_repeats_inner, strata = "y"))
    }
  }
  
  iters <- simplify_cv(iters)
  
}

#FUNCTION: mak_jobs(iters, model_info)--------------------------------
mak_jobs <- function(iters, model_info){
  
  n_iters_inner <- iters$n_folds_inner * iters$n_repeats_inner
  model_types <- model_info$model_types
  features <- model_info$features
  model_name <- model_info$model_name
  raw_data_name <- model_info$raw_data_name
  
  if(iters$cv_type=='single_loop'){
    jobs <- crossing(model_name = model_name, raw_data_name = raw_data_name, iter_inner = 1:n_iters_inner, model_types, features)
    
    jobs$iter_inner_name <- NA_character_
    for (i in 1:nrow(jobs)) {
      jobs$iter_inner_name[i] <- names(iters$inner_in_id)[jobs$iter_inner[i]]
    }
  }
  
  if(iters$cv_type=='nested'){
    n_iters_outer <- iters$n_folds_outer * iters$n_repeats_outer
    
    jobs <- crossing(model_name = model_name, raw_data_name = raw_data_name, iter_outer = 1:n_iters_outer, iter_inner = 1:n_iters_inner, model_types, features)
    
    jobs$iter_outer_name <- NA_character_
    jobs$iter_inner_name <- NA_character_
    for (i in 1:nrow(jobs)) {
      jobs$iter_outer_name[i] <- names(iters$outer_in_id)[jobs$iter_outer[i]]
      jobs$iter_inner_name[i] <- names(iters$inner_in_id[[jobs$iter_outer[i]]])[jobs$iter_inner[i]]
    }
  }
  
  jobs <- jobs %>% 
    mutate(job_num = 0:(nrow(jobs)-1)) %>% 
    select(job_num, everything())
}

#Function: unzip_chtc(model_name, root_path)-------------------

unzip_chtc <-function(model_name, root_path) {
  in_path <- file.path(root_path, model_name, "output")
  
  results <- dir(file.path(root_path, model_name, "output", "results"), pattern = "*.rds", full.names = FALSE)
  if (length(results)==0)  {
    message(str_c("Unzipping results.zip to ", file.path(in_path, "results")))
    unzip(file.path(in_path, "results.zip"), exdir = file.path(in_path, "results"))
  }else{
    message("Results files detected; unzip skipped")
  }
  
  err <- dir(file.path(root_path, model_name, "output", "err"), pattern = "*.err", full.names = FALSE)
  if (length(err)==0)  {
    message(str_c("Unzipping err.zip to ", file.path(in_path, "err")))
    unzip(file.path(in_path, "err.zip"), exdir = file.path(in_path, "err"))
  }else{
    message("Err files detected; unzip skipped")
  }
  
  out <- dir(file.path(root_path, model_name, "output", "out"), pattern = "*.out", full.names = FALSE)
  if (length(out)==0)  {
    message(str_c("Unzipping output.zip to ", file.path(in_path, "output")))
    unzip(file.path(in_path, "out.zip"), exdir = file.path(in_path, "out"))
  }else{
    message("Out files detected; unzip skipped")
  }
}

#Function: boot_mean(d, n_boots, seed=19690127)--------------------------------------
boot_mean <- function (d, n_boots, seed=19690127){
#Returns n_boots means of d and the studentized lower and upper
#95% CI limits
  calc_mean <- function(data, indices){
    data <- data[indices]
    m <- mean(data)
    v <- var(data)
    c(m,v)
  }
  
  set.seed(seed)
  means <- boot(data=d, statistic = calc_mean, R=n_boots)
  ci <- boot.ci(means)
  results = list(means = means$t[,1], mean_samp = ci$t0, lower_basic = ci$basic[4], upper_basic = ci$basic[5], lower_t = ci$student[4], upper_t = ci$student[5])
}


#FUNCTION get_best_cuts (predictions, labels, neg_label)------------------------------------
#used OptimalCutpoints package to select best cutpoints for
#accuracy and ROC01 based on labels and predictions
get_best_cuts <- function(predictions, labels, neg_label) {
  
  d = data.frame(predictions = predictions, labels = labels)
  
  cuts <- optimal.cutpoints(X = "predictions", status = "labels", tag.healthy = neg_label, methods = c("ROC01"), data = d,
                            ci.fit = FALSE)
  
  #Thres for ROC01
  thres_roc01 <- cuts$ROC01$Global$optimal.cutoff$cutoff[1]  #take first if multiple cuts
  
  #Thres for Accuracy
  thres <- tibble(thres = cuts$ROC01$Global$measures.acc$cutoffs) %>% 
    mutate(tp = cuts$ROC01$Global$measures.acc$Se * cuts$ROC01$Global$measures.acc$n$d,
           tn = cuts$ROC01$Global$measures.acc$Sp * cuts$ROC01$Global$measures.acc$n$h,
           fn = cuts$ROC01$Global$measures.acc$n$d - tp,
           fp = cuts$ROC01$Global$measures.acc$n$h - tn,
           n = tp + tn + fn + fp,
           acc = (tp + tn)/n
           )
  thres_acc <- as.numeric(thres$thres[which(thres$acc == max(thres$acc))])
  thres_acc <- thres_acc[1]

  results <- list(roc01 = thres_roc01, acc = thres_acc)
}

#FUNCTION: get_rocr (labels, predictions, folds, pos_label, metric1, metric2)---------------------
#produces a list of n_folds vectors for both labels and predictions and creates
#prediction and then performance class from ROCR for graphing model performance curves over CV
get_rocr <- function(labels, predictions, folds, pos_label, metric1="tpr", metric2="fpr") {
  n_folds <- length(unique(folds))
  prediction_list <- vector("list", n_folds)
  label_list <- vector("list", n_folds)
  
  for(i in 1:n_folds){
    this_fold = unique(folds)[i]  
    prediction_list[[i]] <- predictions[folds==this_fold]
    label_list[[i]] <- labels[folds==this_fold]
  }

  label_names <- levels(labels)
  if (!(length(label_names) == 2)) stop("Labels must have exactly 2 levels")
  label_order <- c(label_names[!(label_names==pos_label)], pos_label)  #order = neg, pos
  
  roc_pred <- prediction(prediction_list, label_list, 
                         label.ordering = label_order)
  roc_perf <- performance(roc_pred, metric1, metric2)
}

#FUNCTION: do_corrected_cv_test(type, pc, pa, n_train, n_test)---------------------------
#does a one tailed test of pa > pc
corrected_cv_test <- function(type = "paired", pc, pa, n_train, n_test){
  results <- NULL

  #for use with paired or on-sample (vs. pop value) tests
  if (tolower(type)=="paired"){
    diffs <- pa - pc
    n <- length(diffs)
    mean_diff <- mean(diffs)
    var_diffs <- var(diffs)
    correction <- 1/n + (n_test/n_train)
    se = sqrt(correction * var_diffs)
    
    #var_corrected <- (1/n + (n_test/n_train)) * var_diffs
    
    t = mean_diff/se
    p_value <- pt(t, n-1, lower.tail = FALSE)
    results <- list(mean = mean_diff, se = se, t=t, df = n - 1, p_one_tail = p_value)
  }

  if (tolower(type)=="independent"){
    mean_pc <- mean(pc)
    mean_pa <- mean(pa)
    n_pc <- length(pc)
    n_pa <- length(pa)
    var_pc <- var(pc)
    var_pa <- var(pa)
    var_pooled <- ((n_pc) * var_pc + (n_pa) * var_pa) / (n_pc + n_pa)
    
    correction <- (1/(n_pc + n_pa) + (n_test/n_train))
    se = sqrt(correction * var_pooled)
    
    t = (mean_pa - mean_pc)/se
    p_value <- pt(t, (n_pc + n_pa) - 2, lower.tail = FALSE)
    results <- list(t=t, df = (n_pc + n_pa) - 2, p_one_tail = p_value)
  }  
  return(results)
}


# #FUNCTION: mak_iters(d, n_folds_outer, n_repeats_outer, n_folds_inner, n_repeats_inner, seed)-------------------
# mak_iters <- function(d, n_folds_outer = 0, n_repeats_outer = 0, n_folds_inner, n_repeats_inner, seed){
#   
#   set.seed(seed)
#   
#   if(n_folds_outer == 0){
#     iters <- vfold_cv(d, v = n_folds_inner, repeats = n_repeats_inner, strata = "y")
#   }
#   if(n_folds_outer > 0){
#     iters <- nested_cv(
#       d, 
#       outside = vfold_cv(v = n_folds_outer, repeats = n_repeats_outer, strata = "y"), 
#       inside = vfold_cv(v = n_folds_inner, repeats = n_repeats_inner, strata = "y"))
#   }
#   
#   iters <- simplify_cv(iters)
#   
# }
# 
# #FUNCTION: mak_jobs(iters, model_info)--------------------------------
# mak_jobs <- function(iters, model_info){
#   
#   n_iters_inner <- iters$n_folds_inner * iters$n_repeats_inner
#   model_types <- model_info$model_types
#   features <- model_info$features
#   model_name <- model_info$model_name
#   raw_data_name <- model_info$raw_data_name
#   
#   if(iters$cv_type=='single_cv'){
#     jobs <- crossing(model_name = model_name, raw_data_name = raw_data_name, iter_inner = 1:n_iters_inner, model_types, features)
#     
#     jobs$iter_inner_name <- NA_character_
#     for (i in 1:nrow(jobs)) {
#       jobs$iter_inner_name[i] <- names(iters$inner_in_id)[jobs$iter_inner[i]]
#     }
#   }
#   if(iters$cv_type=='nested'){
#     n_iters_outer <- iters$n_folds_outer * iters$n_repeats_outer
#     
#     jobs <- crossing(model_name = model_name, raw_data_name = raw_data_name, iter_outer = 1:n_iters_outer, iter_inner = 1:n_iters_inner, model_types, features)
#     
#     jobs$iter_outer_name <- NA_character_
#     jobs$iter_inner_name <- NA_character_
#     for (i in 1:nrow(jobs)) {
#       jobs$iter_outer_name[i] <- names(iters$outer_in_id)[jobs$iter_outer[i]]
#       jobs$iter_inner_name[i] <- names(iters$inner_in_id[[jobs$iter_outer[i]]])[jobs$iter_inner[i]]
#     }
#   }
#   jobs
# }
