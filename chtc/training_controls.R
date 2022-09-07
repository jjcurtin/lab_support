# This is a study level script for defining the recipe and global attributes that are 
# the same across jobs.

# DO NOT EDIT DIRECTLY IN THIS SCRIPT - this script serves as a template with all 
# possible parameters specified/demo'd

#EDIT THIS
study <- "messages"
data_type <- "all"   # but still need to change more (e.g., feature set) to switch data_type
window <- "1week"
lead <- 0
version <- "v1"
algorithm <- c("glmnet", "knn", "random_forest", "xgboost") # 1+ algorithm (glmnet, random_forest) 




# SET GLOBAL PARAMETERS --------
feature_set <- c("feat_baseline_id", "feat_baseline_temporal") # 1+ feature sets
data_trn <- str_c("features_", data_type, "_", window, "_", lead, "_", version, ".csv.xz")
resample <- c("none", "up_1", "down_1", "smote_1") # 1+ resampling methods (up, down, smote, or none).  All resamples should be in form resample type underscore under_ratio (e.g., 3 = 25% minority cases)
y_col_name <- "label" # outcome variable - will be changed to y in recipe for consistency across studies 
remove_nzv <- TRUE # using as variable instead of in recipe to be able to calculate number of features before removing nzv


# CV PARAMETERS
# INSERT INSTRUCTIONS HERE
cv_resample_type <- "nested" 
cv_resample = NULL
cv_inner_resample <- "1_x_10" # can also be a single number for bootstrapping (i.e., 100)
cv_outer_resample <- "1_x_10" 
cv_group <- "subid" # remove or set to NULL if not grouping


# SET STUDY PATHS
name_job <- str_c("train_", window, "_", lead, "_", version, "_", algorithm) # the name of the job to set folder names
path_jobs <- str_c("P:/studydata/risk/chtc/", study) # location of where you want your jobs to be setup
path_data <- str_c("P:/studydata/risk/data_processed/", study) # location of data set
path_project <- "./meta/ana_scripts"   # NULL if using staging data


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
tar <- c("chtc_train.tar.gz", "meta.tar.gz") # name of tar packages for submit file - does not transfer these anywhere 
max_idle <- 1000 # according to CHTC we should set this at 1000 to not flood the server. It will not limit the number of jobs running at one time 
request_cpus <- 1 
request_memory <- "8000MB"
request_disk <- "1000000KB" # this is pretty large - necessary for meta
flock <- FALSE
glide <- FALSE


# BUILD RECIPE ---------

# Script should have a single build_recipe function to be compatible with fit script. 
# Use if statements to customize recipes instead of using multiple different recipe functions.  


# Sample recipe from meta project below - this is for fitting classification models
build_recipe <- function(d, job) {
  
  # d: (training) dataset from which to build recipe
  # job: single-row job-specific tibble
  
  # get relevant info from job (algorithm, feature_set, resample, under_ratio)
  algorithm <- job$algorithm
  feature_set <- job$feature_set
  
  if (job$resample == "none") {
    resample <- job$resample
  } else {
    resample <- str_split(job$resample, "_")[[1]][1]
    under_ratio <- as.numeric(str_split(job$resample, "_")[[1]][2])
  }
  
  # Set recipe steps generalizable to all model configurations
  rec <- recipe(y ~ ., data = d) %>%
    step_rm(label_num, subid, dttm_label) %>% 
    step_string2factor(y, levels = c("yes", "no")) %>% # positive case should be first
    # reference group will be first level in factor - specify levels to choose reference group
    step_string2factor(label_weekday, levels = c("Mon", "Tues", "Wed", "Thu", "Fri", "Sat", "Sun")) %>%
    step_num2factor(label_hour, levels = c("4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
                                           "14", "15", "16", "17", "18", "19", "20", "21", "22",
                                           "23", "24", "1", "2", "3")) %>%
    step_string2factor(label_season, levels = c("Spring", "Summer", "Fall", "Winter")) %>% 
    step_string2factor(all_nominal()) %>% 
    step_zv(all_predictors()) %>% 
    step_impute_median(all_numeric()) %>% 
    step_impute_mode(all_nominal(),  -y) 
  
  # If statements for filtering features based on feature set
  if (feature_set == "feat_all_passive") {
    rec <- rec %>%
      step_rm(starts_with("sms"), -contains("passive")) %>% 
      step_rm(starts_with("voi"), -contains("passive"))
  } else if (feature_set == "feat_baseline_id") {
    rec <- rec %>% 
      step_rm(starts_with("sms")) %>% 
      step_rm(starts_with("voi")) %>% 
      step_rm(starts_with("label"))
  } else if (feature_set == "feat_baseline_temporal") {
    rec <- rec %>% 
      step_rm(starts_with("id")) %>% 
      step_rm(starts_with("sms")) %>% 
      step_rm(starts_with("voi")) 
  } else if (feature_set == "feat_baseline_all") {
    rec <- rec %>% 
      step_rm(starts_with("sms")) %>% 
      step_rm(starts_with("voi")) 
  } else if (feature_set == "feat_logs") {
    rec <- rec %>% 
      step_rm(starts_with("id")) %>% 
      step_rm(starts_with("label"))
  }
  
  # resampling options for unbalanced outcome variable
  if (resample == "down") {
    rec <- rec %>% 
      themis::step_downsample(y, under_ratio = under_ratio, seed = 10) 
  } else if (resample == "smote") {
    if (under_ratio != 1) { over_ratio <- under_ratio / (under_ratio + 1)
    } else over_ratio <- under_ratio
    rec <- rec %>% 
      themis::step_smote(y, over_ratio = over_ratio, seed = 10) 
  } else if (resample == "up") {
    if (under_ratio != 1) { over_ratio <- under_ratio / (under_ratio + 1)
    } else over_ratio <- under_ratio
    rec <- rec %>% 
      themis::step_upsample(y, over_ratio = over_ratio, seed = 10)
  }
  
  # algorithm specific steps
  if (algorithm == "glmnet") {
    rec <- rec  %>%
      step_dummy(all_nominal(), -y) %>%
      step_normalize(all_predictors()) %>% 
      # drop columns with NA values after imputation (100% NA)
      step_select(where(~ !any(is.na(.))))
      # step nzv done within fit if training controls remove_nzv is set to TRUE
  } 
  
  if (algorithm == "knn") {
    rec <- rec  %>% 
      step_dummy(all_nominal(), -y) %>% 
      step_normalize(all_predictors()) %>% 
      # drop columns with NA values after imputation (100% NA)
      step_select(where(~ !any(is.na(.))))
  } 
  
  return(rec)
}


