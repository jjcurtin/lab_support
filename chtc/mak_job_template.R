# setup chtc jobs & associated files/folders

# NOTE: this script should be run from a project on the repo (i.e., RISK or RISK2) 
# for the relative paths to access local copy of lab_support to work

# CHANGE GLOBAL JOB PARAMETERS -------------------
data_trn <- "period_720_lead_0.csv"
name_job <- "glmnet_knn_rf" # the name of the job to set folder names
feature_set <- c("feat_baseline_id", "feat_baseline_temporal", "feat_all", "feat_all_passive") # 1+ feature sets to  use
algorithm <- c("glmnet", "knn", "random_forest") # 1+ algorithm (glmnet, random_forest) 
resample <- c("none", "up_1", "down_1", "smote_1") # 1+ resampling methods (up, down, smote, or none)
# all resamples should be in form resample type underscore under_ratio (e.g., 3 = 25% minority cases)
cv_type <- "group_kfold_1_x_10" # (boot, group_kfold, kfold)
# format for kfold should be kfold_n_repeats_x_n_folds (e.g., kfold_1_x_10, group_kfold_10_x_10)
# determine where to pass in global cv_type parameter

# CHANGE ALGORITHM-SPECIFIC HYPERPARAMETERS -------------------
hp1_glmnet <- seq(0.5, 1, length.out = 11) # alpha (mixture) 
hp1_knn <- seq(5, 75, length.out = 15) # neighbors
hp1_rf <- c(5, 10, 20, 50) # mtry (p/3 for reg or square root of p for class)
hp2_rf <- c(2, 10, 20) # min_n
hp3_rf <- 2800 # trees (10 x's number of predictors)

# CHANGE STUDY PATHS -------------------- 
path_jobs <- "P:/studydata/risk/chtc/meta/jobs" 
path_data <- "P:/studydata/risk/data_processed/meta/features"


# Make jobs -------------------- 
suppressPackageStartupMessages(library(tidyverse))
source("../lab_support/chtc.R", echo = FALSE)

# Don't forget to add/remove hyperparameters using/not using for algorithms
make_jobs(data_trn, name_job, feature_set, algorithm, resample, cv_type,
          path_jobs, path_data, hp_1_glmnet, hp1_knn, hp1_rf, hp2_rf,
          hp3_rf)


