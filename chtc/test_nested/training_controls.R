# Test training controls for nested cv
# 9/8/22


#EDIT THIS
study <- "test_nested"
# data_type <- "all"   # but still need to change more (e.g., feature set) to switch data_type
# window <- "1week"
# lead <- 0
# version <- "v1"
algorithm <- "glmnet"


# SET GLOBAL PARAMETERS --------
feature_set <- c("feat_all") # 1+ feature sets
data_trn <- "test_data.csv" # set to NULL if using chtc staging for large data
resample <- c("none") # 1+ resampling methods (up, down, smote, or none).  All resamples should be in form resample type underscore under_ratio (e.g., 3 = 25% minority cases)
y_col_name <- "y" # outcome variable - will be changed to y in recipe for consistency across studies 
remove_nzv <- TRUE # using as variable instead of in recipe to be able to calculate number of features before removing nzv


# CV PARAMETERS
# All cv parameters must have a value or be set to NULL
# cv_resample will be used to specify kfold and bootstrapping splits (non-nested)
# nested cv should use cv_inner_resample and cv_outer_resample instead of cv_resample
# see resampling demo in lab_support/chtc for examples
cv_resample_type <- "nested" # can be boot, kfold, or nested
cv_resample = NULL # can be repeats_x_folds (e.g., 1_x_10, 10_x_10) or number of bootstraps (e.g., 100)
cv_inner_resample <- "1_x_10" # can also be a single number for bootstrapping (i.e., 100)
cv_outer_resample <- "1_x_10" # outer resample will always be kfold
cv_group <- "subid" # set to NULL if not grouping


# SET STUDY PATHS
name_job <- str_c("train_", algorithm) # the name of the job to set folder names
path_jobs <- str_c("P:/studydata/test_nested/chtc/") # location of where you want your jobs to be setup
path_data <- str_c("P:/studydata/test_nested/data_processed/") # location of data set


# SET ALGORITHM-SPECIFIC HYPERPARAMETERS
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
hp3_xgboost <- c(20, 30, 40, 50)  # mtry (previously included 2 and 10 but not needed)
# trees = 100
# early stopping = 10



# CHANGE CHTC SPECIFIC CONTROLS
tar <- c("train.tar.gz") # name of tar packages for submit file - does not transfer these anywhere 
max_idle <- 1000 # according to CHTC we should set this at 1000 to not flood the server. It will not limit the number of jobs running at one time 
request_cpus <- 1 
request_memory <- "8000MB"
request_disk <- "1000000KB" # this is pretty large - necessary for meta
flock <- FALSE
glide <- FALSE


# BUILD RECIPE ---------

# Script should have a single build_recipe function to be compatible with fit script. 
# Use if statements to customize recipes instead of using multiple different recipe functions.  


build_recipe <- function(d, job) {
  
  # d: (training) dataset from which to build recipe
  # job: single-row job-specific tibble
  
  # get relevant info from job (algorithm, feature_set, resample, under_ratio)
  algorithm <- job$algorithm
  
# Set recipe steps generalizable to all model configurations
  rec <- recipe(y ~ ., data = d) %>%
    step_rm(subid)
  
  # algorithm specific steps
  if (algorithm == "glmnet" | algorithm == "knn") {
    rec <- rec  %>%
      step_normalize(all_predictors()) 
      # step nzv done within fit if training controls remove_nzv is set to TRUE
  } 
  
  
  return(rec)
}


