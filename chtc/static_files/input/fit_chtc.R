# fit model at chtc 

# libraries & source functions file ----------------
suppressPackageStartupMessages({
  require(dplyr)
  require(vroom)
  require(tidyr)
  require(stringr)
}) 
source("fun_chtc.R")
source("training_controls.R")

# set up job_num ---------------
# job_num_arg <- 1
args <- commandArgs(trailingOnly = TRUE) 
job_num_arg <- args[1]

# read in jobs.csv file ------------------
jobs <- read_csv("jobs.csv", col_types = "iiiccdddc")

# pull out job ------------------
job <- jobs %>% 
  filter(job_num == job_num_arg)

# read in data train --------------- 
fn <- str_subset(list.files(), "^data_trn")
if (str_detect(fn, ".rds")) {
  d <- read_rds(fn)
} else {
  d <- vroom(fn, show_col_types = FALSE) 
}

# Set outcome variable to y
d <- d %>% 
  rename(y = {{y_col_name}})

# create splits object ---------------
set.seed(102030)
splits <- if (str_split(str_remove(cv_type, "_x"), "_")[[1]][1] == "group") {
  make_splits(d = d, cv_type = cv_type, group = group)
} else { 
  make_splits(d = d, cv_type = cv_type)
}

# build recipe ----------------
rec <- build_recipe(d = d, job = job)

# make features on d to get n_feats ----------------
# make features before removing nzv
feat_all <-  rec %>% 
  # remove id variables from count
  step_rm(has_role(match = "id variable")) %>% 
  prep(training = d, strings_as_factors = FALSE) %>% 
  bake(new_data = NULL)

# remove nzv if specified in training controls
if (remove_nzv & job$algorithm == "glmnet") {
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
  write_csv(str_c("results_", job$job_num, ".csv"))

