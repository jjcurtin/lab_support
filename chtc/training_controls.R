# Training controls for EMA study

# FORMAT PATH FUNCTION------
source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")

# SET GLOBAL PARAMETERS------
study <- "ema"
window <- "1hour"
lead <- 0
version <- "v4"
algorithm <- "xgboost"
batch <- "batch5"

configs_per_job <- 50  # number of model configurations that will be fit/evaluated within each CHTC


# RESAMPLING FOR OUTCOME------
# note that ratio is under_ratio, which is used by downsampling as is
# It is converted to  overratio (1/ratio) for up and smote
resample <- c("down_3") 
# resample <- c("down_1", "up_1", "smote_1", "down_2", "up_.5", "smote_.5", down_3) 


# DATA, SPLITS AND OUTCOME------
feature_set <- c("all") # EMA Features set names
data_trn <- str_c("features_",  window, "_", lead, "_", version, ".csv.xz") # Set to NULL if staging
seed_splits <- 102030

ml_mode <- "classification"   # regression or classification
y_col_name <- "lapse" 
y_level_pos <- "yes" 
y_level_neg <- "no"


# CV SETTINGS------
cv_resample_type <- "nested" # can be boot, kfold, or nested
cv_resample = NULL # can be repeats_x_folds (e.g., 1_x_10, 10_x_10) or number of bootstraps (e.g., 100)
cv_inner_resample <- "3_x_10" # can also be a single number for bootstrapping (i.e., 100)
cv_outer_resample <- "3_x_10" # outer resample will always be kfold
cv_group <- "subid" # set to NULL if not grouping

cv_name <- if_else(cv_resample_type == "nested",
                   str_c(cv_resample_type, "_", cv_inner_resample, "_",
                         cv_outer_resample),
                   str_c(cv_resample_type, "_", cv_resample))

# STUDY PATHS------
# the name of the batch of jobs to set folder name
name_batch <- str_c("train_", algorithm, "_", window, "_", cv_name, "_", version, "_", batch) 
# the path to the batch of jobs
path_batch <- format_path(str_c("studydata/risk/chtc/", study, "/", name_batch)) 
# location of data set
path_data <- format_path(str_c("studydata/risk/data_processed/", study)) 


# ALGORITHM-SPECIFIC HYPERPARAMETERS------
hp1_glmnet <- c(0.05, seq(.1, 1, length.out = 10)) # alpha (mixture)
hp2_glmnet_min <- -8 # min for penalty grid - will be passed into exp(seq(min, max, length.out = out))
hp2_glmnet_max <- 2 # max for penalty grid
hp2_glmnet_out <- 200 # length of penalty grid

hp1_knn <- seq(5, 255, length.out = 26) # neighbors (must be integer)

hp1_rf <- c(2, 10, 20, 30, 40) # mtry (p/3 for reg or square root of p for class)
hp2_rf <- c(2, 15, 30) # min_n
hp3_rf <- 1500 # trees (10 x's number of predictors)

hp1_xgboost <- c(0.0001, 0.001, 0.01, 0.1, 0.2, 0.3, .4)  # learn_rate
hp2_xgboost <- c(1, 2, 3, 4) # tree_depth
hp3_xgboost <- c(20, 30, 40, 50)  # mtry
# trees = 500
# early stopping = 20

hp1_rda <- seq(.1, 1, length.out = 10)  # frac_common_cov: Fraction of the Common Covariance Matrix (0-1; 1 = LDA, 0 = QDA)
hp2_rda <- seq(.1, 1, length.out = 10) # frac_identity: Fraction of the Identity Matrix (0-1)
 
hp1_nnet <- seq(10, 100, length.out = 10)  # epochs
hp2_nnet <- seq(0, 0.1, length.out = 100) # penalty
hp3_nnet <- seq(5, 30, length.out = 5) # hidden units

# CHTC SPECIFIC CONTROLS------
username <- "jjcurtin" # for setting staging directory (until we have group staging folder)
stage_data <- TRUE
max_idle <- 1000
request_cpus <- 1 
request_memory <- "25000MB"
request_disk <- "1600MB"
flock <- FALSE
glide <- FALSE

# Batches
# down_1: request_memory <- "24000MB", request_disk <- "1600MB" john
# down_2: request_memory <- "24000MB", request_disk <- "1600MB" susan
# up_.5: request_memory <- "28000MB", request_disk <- "1600MB" john (not complete)
# up_1: request_memory <- "30000MB", request_disk <- "1600MB" kendra (not complete)
# down_3:

# FORMAT DATA------
format_data <- function (df){
  
  df %>% 
    rename(y = !!y_col_name) %>% 
    mutate(y = factor(y, levels = c(!!y_level_pos, !!y_level_neg)), # set pos class first
           across(where(is.character), factor)) %>%
    select(-label_num, -dttm_label)
  # Now include additional mutates to change classes for columns as needed
  # see https://jjcurtin.github.io/dwt/file_and_path_management.html#using-a-separate-mutate
}


# BUILD RECIPE------
# Script should have a single build_recipe function to be compatible with fit script. 
build_recipe <- function(d, config) {
  # d: (training) dataset from which to build recipe
  # config: single-row config-specific tibble
  
  # get relevant info from job (algorithm, feature_set, resample, under_ratio)
  algorithm <- config$algorithm
  feature_set <- config$feature_set
  
  if (config$resample == "none") {
    resample <- config$resample
  } else {
    resample <- str_split(config$resample, "_")[[1]][1]
    ratio <- as.numeric(str_split(config$resample, "_")[[1]][2])
  }
  
  # Set recipe steps generalizable to all model configurations
  rec <- recipe(y ~ ., data = d) %>%
    step_rm(subid) %>%
    step_zv(all_predictors()) %>% 
    step_impute_median(all_numeric_predictors()) %>% 
    step_impute_mode(all_nominal_predictors()) 

  
  # resampling options for unbalanced outcome variable
  if (resample == "down") {
    # under_ratio = ratio.  No conversion needed
    rec <- rec %>% 
      themis::step_downsample(y, under_ratio = ratio, seed = 10) 
  }
  
  if (resample == "smote") {
    # correct ratio to over_ratio
    rec <- rec %>% 
      themis::step_smote(y, over_ratio = 1 / ratio, seed = 10) 
  }
  
  if (resample == "up") {
    # correct ratio to over_ratio
    rec <- rec %>% 
      themis::step_upsample(y, over_ratio = 1 / ratio, seed = 10)
  }
  
  # algorithm specific steps
  if (algorithm == "glmnet") {
    rec <- rec  %>%
      step_dummy(all_nominal_predictors()) %>%
      step_normalize(all_predictors())
  } 
  
  if (algorithm == "random_forest") {
    # no algorithm specific steps
  } 
  
  if (algorithm == "xgboost") {
    rec <- rec  %>% 
      step_dummy(all_nominal_predictors())
  } 
  
  # final steps for all algorithms
  rec <- rec %>%
    # drop columns with NA values after imputation (100% NA)
    step_select(where(~ !any(is.na(.)))) %>%
    step_nzv()
  
  return(rec)
}

# Update paths for OS------
# This does NOT need to be edited.  This will work for Windows, Mac and Linux OSs
path_batch <- case_when(Sys.info()[["sysname"]] == "Windows" ~str_c("P:/", path_batch),
                        Sys.info()[["sysname"]] == "Linux" ~str_c("~/mnt/private/", path_batch),
                        .default = str_c("/Volumes/private/", path_batch))

path_data <- case_when(Sys.info()[["sysname"]] == "Windows" ~str_c("P:/", path_data),
                       Sys.info()[["sysname"]] == "Linux" ~str_c("~/mnt/private/", path_data),
                       .default = str_c("/Volumes/private/", path_data))