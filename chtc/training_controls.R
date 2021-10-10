# This is a study level script for defining the recipe and global attributes that are 
# the same across jobs.

# Note: Should have a single build_recipe function to be compatible with fit script.   


# SET GLOBAL PARAMETERS --------
data_trn <- "period_168_lead_0.csv"
name_job <- "test" # the name of the job to set folder names
feature_set <- c("feat_baseline_id", "feat_baseline_temporal", "feat_all", "feat_all_passive") # 1+ feature sets to  use
algorithm <- c("glmnet", "knn", "random_forest") # 1+ algorithm (glmnet, random_forest) 
resample <- c("none", "up_1", "down_1", "smote_1") # 1+ resampling methods (up, down, smote, or none)
# all resamples should be in form resample type underscore under_ratio (e.g., 3 = 25% minority cases)
y <- "y" # outcome variable - will be changed to y in recipe for consistency across studies 
cv_type <- "group_kfold_1_x_10" # cv type - can be boot, group_kfold, or kfold
# format for kfold should be kfold_n_repeats_x_n_folds (e.g., kfold_1_x_10, group_kfold_10_x_10)
# determine where to pass in global cv_type parameter

# CHANGE ALGORITHM-SPECIFIC HYPERPARAMETERS -------------------
# Can remove or comment out hyperparameter variables if not using the algorithm 
# if using the algorithm, you must provide the associated hyperparameters
hp1_glmnet <- seq(0.5, 1, length.out = 11) # alpha (mixture) 
hp2_glmnet_min <- -8 # min for penalty grid - will be passed into exp(seq(min, max, length.out = out))
hp2_glmnet_max <- 2 # max for penalty grid
hp2_glmnet_out <- 100 # length of penalty grid
hp1_knn <- seq(5, 75, length.out = 15) # neighbors
hp1_rf <- c(5, 10, 20, 50) # mtry (p/3 for reg or square root of p for class)
hp2_rf <- c(2, 10, 20) # min_n
hp3_rf <- 2800 # trees (10 x's number of predictors)

# CHANGE STUDY PATHS -------------------- 
path_jobs <- "P:/studydata/risk/chtc/meta/jobs" # location of where you want your jobs to be setup
path_data <- "P:/studydata/risk/data_processed/meta/features" # location of data set




# BUILD RECIPE ---------

# Sample recipe from meta project - this is for fitting classification models

build_recipe <- function(d, job, y) {
  
  # d: (training) dataset from which to build recipe
  # job: single-row job-specific tibble
  # y = binary outcome variable (yes/no)
  # feature_set = feat_baseline_id, feat_baseline_temporal, feat_all, feat_all_passive
  # resample = type + under_ratio or none
  
  # get relevant info from jobs file (algorithm, feature_set, resample, under_ratio)
  algorithm <- job$algorithm
  feature_set <- job$feature_set
  
  if (job$resample == "none") {
    resample <- job$resample
  } else {
    resample <- str_split(job$resample, "_")[[1]][1]
    under_ratio <- as.numeric(str_split(job$resample, "_")[[1]][2])
  }
  
  # Set outcome variable to y
  d <- d %>% 
    rename(y = y)
  
  # Set recipe steps generalizable to all model configurations
  rec <- recipe(y ~ ., data = d) %>%
    step_string2factor(y, levels = c("no", "yes")) %>% 
    update_role(subid, dttm_label, new_role = "id variable") %>%
    # still working on figuring out how to set reference level
    # step_string2factor(label_weekday, levels = c("Mon", "Tues", "Wed", "Thu", "Fri", "Sat", "Sun")) %>% 
    # step_string2factor(label_hour, levels = c("4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
    #                                        "14", "15", "16", "17", "18", "19", "20", "21", "22",
    #                                        "23", "24", "1", "2", "3")) %>% 
    step_string2factor(all_nominal()) %>% 
    step_impute_median(all_numeric()) %>% 
    step_impute_mode(all_nominal(), -y) %>% 
    step_zv(all_predictors()) %>% 
    step_dummy(all_nominal(), -y) # reference variable is first level in factor
  
  
  # If statements for filtering features based on feature set
  if (feature_set == "feat_all_passive") {
    rec <- rec %>%
      step_rm(starts_with("context"))
  } else if (feature_set == "feat_baseline_id") {
    rec <- rec %>% 
      step_rm(starts_with("sms")) %>% 
      step_rm(starts_with("voice")) %>% 
      step_rm(starts_with("all")) %>% 
      step_rm(starts_with("context"))
  } else if (feature_set == "feat_baseline_temporal") {
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
  if (algorithm == "glmnet" | algorithm == "knn") {
    rec <- rec %>% 
      step_normalize(all_predictors())
  } 
  
  return(rec)
}

