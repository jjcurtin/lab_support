# Fit model at chtc

# libraries & source functions file ----------------
suppressWarnings(suppressPackageStartupMessages({
  require(dplyr)
  # require(vroom) 
  require(tidyr)
  require(stringr)
  require(readr)
})) 
source("fun_chtc.R")
source("training_controls.R")  

# set up job ---------
# job_num_arg <- 1
args <- commandArgs(trailingOnly = TRUE) 
job_num_arg <- args[1]

job <- read_csv("jobs.csv", col_types = "iiiiccdddc") %>% 
  filter(job_num == job_num_arg)

# Read in data train --------------- 
fn <- str_subset(list.files(), "^data_trn")
if (str_detect(fn, ".rds")) {
  d <- read_rds(fn)
} else {
  d <- read_delim(fn, show_col_types = FALSE) # supports both csv and tsv formats
}

# Format data------------------------
# change column classes, rename Y, etc
# This is a custom/study specific function that exists in training_controls
d <- format_data(d)  
 

# Create nested outer splits object ---------------
splits <- d %>% 
  make_splits(cv_resample_type, cv_resample, cv_outer_resample, 
              cv_inner_resample, cv_group, the_seed = seed_splits)

# Build recipe ----------------
# This is a custom/study specific function that exists in training_controls
rec <- build_recipe(d = d, job = job)
rm(d) # no longer need d


# Fit model and get predictions and model metrics ----------------
results <- if (job$algorithm == "glmnet") {
  tune_model(job = job, rec = rec, splits = splits, ml_mode = ml_mode, 
             cv_resample_type = cv_resample_type, 
             hp2_glmnet_min = hp2_glmnet_min, hp2_glmnet_max = hp2_glmnet_max, 
             hp2_glmnet_out = hp2_glmnet_out,
             y_level_pos = y_level_pos)
} else {
  tune_model(job = job, rec = rec, splits = splits, 
             cv_resample_type = cv_resample_type, ml_mode = ml_mode, 
             y_level_pos = y_level_pos)
}

# write out results tibble ------------
 results %>% 
  mutate(job_num = job$job_num) %>% 
  relocate(job_num) %>% 
  write_csv(str_c("results_", job$job_num, ".csv"))

