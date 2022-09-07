# Do Nested CV on CHTC

# libraries & source functions file ----------------
suppressWarnings(suppressPackageStartupMessages({
  require(dplyr)
  require(vroom) 
  require(tidyr)
  require(stringr)
  require(readr)
})) 
source("fun_chtc.R")
# source("training_controls.R")   # NEED TO UPDATE TRAINING CONTROLS FOR NEW PARAMETERS (SEE BELOW)

# set up job---------
# job_num_arg <- 3, 47, 91
args <- commandArgs(trailingOnly = TRUE) 
job_num_arg <- args[1]

jobs <- vroom("jobs.csv", col_types = "iiiccdddc")

job <- jobs %>% 
  filter(job_num == job_num_arg)

# read in data train --------------- 
# fn <- str_subset(list.files(), "^data_trn")
# if (str_detect(fn, ".rds")) {
#   d <- read_rds(fn)
# } else {
#   d <- vroom(fn, show_col_types = FALSE) 
# }
# 
# # Set outcome variable to y
# d <- d %>% 
#   rename(y = {{y_col_name}})
 
# TEMP FAKE DATA and stuff from training controls
# KENDRA CREATE ON SERVER IN data_processed
# d <- tibble(subid = sort(rep(1:30, times = 10)), x1 = rnorm(300), x2 = rnorm(300), y = rbinom(300,1,.5))


# create nested outer splits object ---------------
set.seed(102030)
splits <- d %>% 
  make_splits(cv_resample_type, cv_resample, cv_outer_resample, cv_inner_resample, cv_group)


# build recipe ----------------
# rec <- recipe(y ~ ., data = d)   # KEDRA MOVE TO TRAINING CONTROLS
rec <- build_recipe(d = d, job = job)


# remove nzv if specified in training controls
if (remove_nzv) {
  rec <- rec %>% 
    step_nzv(all_predictors())
}

# fit model and get predictions and model metrics ----------------
results <- if (job$algorithm == "glmnet") {
  tune_model(job = job, rec = rec, folds = splits, cv_type = cv_type, 
             hp2_glmnet_min, hp2_glmnet_max, hp2_glmnet_out)
} else {
  tune_model(job = job, rec = rec, folds = splits, cv_type = cv_type)
}

# write out results tibble ------------
results %>% 
  mutate(n_feats = ncol(feat_all) - 1) %>% # subtract one for y
  mutate(job_num = job$job_num) %>% 
  relocate(job_num) %>% 
  vroom_write(str_c("results_", job$job_num, ".csv"), delim = ",")

