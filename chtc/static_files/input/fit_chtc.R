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

# write out results tibble ------------
results %>% 
  write_csv(., str_c("results_", process_num, ".csv"))

