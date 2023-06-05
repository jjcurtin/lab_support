# Fit model at chtc

# libraries & source functions file ----------------
suppressWarnings(suppressPackageStartupMessages({
  require(dplyr)
  require(tidyr)
  require(stringr)
  require(readr)
  require(recipes)
  require(parsnip)
  require(themis)
  require(tune)
  require(yardstick)
  require(rsample)
  require(ranger)
  require(glmnet)
  require(kknn)
})) 
source("fun_chtc.R")
source("training_controls.R")  

# set up job ---------
# for testing:
#   job_num_arg <- 1
#   config_start_arg <- 1
#   config_end_arg <- 2
args <- commandArgs(trailingOnly = TRUE) 
job_num_arg <- args[1]
config_start_arg <- args[2]
config_end_arg <- args[3]

configs <- read_csv("configs.csv", col_types = "iiiiccdddc")

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


# function to fit and evaluate a model configuration from configs
fit_eval <- function(config_current, configs, d, splits) {
  
  # get current model config
  config <- configs %>%
    filter(config_num == config_current)
  
  # create recipe
  # This is a custom/study specific function that exists in training_controls
  rec <- build_recipe(d = d, config = config)
  
  
  # Fit model and get predictions and model metrics
  results <- if (config$algorithm == "glmnet") {
    tune_model(config = config, rec = rec, splits = splits, ml_mode = ml_mode, 
               cv_resample_type = cv_resample_type, 
               hp2_glmnet_min = hp2_glmnet_min, hp2_glmnet_max = hp2_glmnet_max, 
               hp2_glmnet_out = hp2_glmnet_out,
               y_level_pos = y_level_pos)
  } else {
    tune_model(config = config, rec = rec, splits = splits, 
               cv_resample_type = cv_resample_type, ml_mode = ml_mode, 
               y_level_pos = y_level_pos)
  }
  
  return(results)
}

config_start_arg:config_end_arg %>%
  map(\(config_current) fit_eval(config_current, configs, d, splits)) %>%
  list_rbind() %>%
  write_csv(str_c("results_", job_num_arg, ".csv"))


