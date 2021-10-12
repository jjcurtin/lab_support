# fit model at chtc 

# libraries & source functions file ----------------
suppressPackageStartupMessages({
  require(dplyr)
  require(readr)
  require(tidyr)
  require(stringr)
})
source("fun_chtc.R")
source("training_controls.R")

# set up job_num ---------------
# process_num <- 1
args <- commandArgs(trailingOnly = TRUE) 
process_num <- as.numeric(args[1]) + 1 # CHTC arg starts at 0

# read in jobs.csv file ------------------
jobs <- read_csv("jobs.csv", col_types = cols()) 

# pull out job ------------------
job <- slice(jobs, process_num)

# read in data train --------------- 
d <- read_csv("data_trn.csv", col_types = cols())

# create splits object ---------------
set.seed(102030)
splits <- if (str_split(str_remove(cv_type, "_x"), "_")[[1]][1] == "group") {
  make_splits(d = d, cv_type = cv_type, group = group)
} else { 
  make_splits(d = d, cv_type = cv_type)
}

# build recipe ----------------
rec <- build_recipe(d = d, job = job)

# fit model and get predictions and model metrics ----------------
results <- if (job$algorithm == "glmnet") {
  tune_model(job = job, rec = rec, folds = splits, cv_type = cv_type, 
             hp2_glmnet_min, hp2_glmnet_max, hp2_glmnet_out)
} else {
  tune_model(job = job, rec = rec, folds = splits, cv_type = cv_type)
}

# separate predictions from results ----------------
predictions <- results[[2]]

# add subids by row number for glmnet only (row id = row id in original dataset)
# subids already present for single split models (knn, rf)
if (job$algorithm == "glmnet") {
  predictions <- predictions %>% 
    left_join(d %>% 
                rowid_to_column() %>% 
                select(rowid, subid, dttm_label), by = c(".row" = "rowid")) %>% 
    mutate(job_num = job$job_num)
}

# pull out results from list ----------------
results <- results[[1]]

# write out results tibble ------------
results %>% 
  write_csv(., str_c("results_", process_num, ".csv"))

# Save model ------------
# save as rds due to large file size
predictions %>%
  saveRDS(., str_c("preds_", process_num, ".rds"))
