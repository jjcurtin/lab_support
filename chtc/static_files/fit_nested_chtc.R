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
source("training_controls.R")

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
d <- tibble(subid = sort(rep(1:30, times = 10)), x1 = rnorm(300), x2 = rnorm(300), y = rbinom(300,1,.5))

# START WITH THINKING ABOUT NAMES FOR THIS TYPE OF CV
cv_type <- "nested"
group <- "subid"

# create nested outer splits object ---------------
set.seed(102030)
splits <- if (str_detect(cv_type, "group")) {
  make_splits(d = d, cv_type = cv_type, group = group)
} else { 
  make_splits(d = d, cv_type = cv_type)
}

# build recipe ----------------
rec <- recipe(y ~ ., data = d)   # TEMP
#rec <- build_recipe(d = d, job = job)


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

