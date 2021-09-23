# setup chtc jobs & associated files/folders
# the first two sections should be changed for each job

# NOTE: this script assumes lab_support on the p drive contains chtc.R

# CHANGE GLOBAL JOB PARAMETERS -------------------
data_trn <- "period_720_lead_0.csv"
name_job <- "glmnet_knn" # the name of the job to set folder names
feature_set <- c("all_features") # 1+ data stream to use (all_features or passive_only)
algorithm <- c("glmnet", "knn") # 1+ algorithm (glmnet, random_forest) 
resample <- c("none", "up_3", "up_1", "down_3", "down_1", "smote_3", "smote_1") # 1+ resampling methods (up, down, smote, or none)
# all resamples should be in form resample type underscore under_ratio (e.g., 3 = 25% minority cases)
cv_type <- "group_kfold_2_x_10" # (boot, group_kfold, kfold)
# format for kfold should be kfold_n_repeats_x_n_folds (e.g., kfold_1_x_10, group_kfold_10_x_10)
# determine where to pass in global cv_type parameter

# CHANGE ALGORITHM-SPECIFIC HYPERPARAMETERS -------------------
hp1_glmnet <- seq(0.5, 1, length.out = 11) # alpha (mixture) 
hp1_knn <- seq(5, 75, length.out = 15) # neighbors
# hp1_rf <- c(5, 10, 20, 50) # mtry (p/3 for reg or square root of p for class)
# hp2_rf <- c(2, 10, 20) # min_n
# hp3_rf <- 2000 # trees (10 x's number of predictors)

# set paths -------------------- 
path_jobs <- "P:/studydata/risk/chtc/meta/jobs" 
path_templates <- "templates"
path_data <- "P:/studydata/risk/data_processed/meta/features"
path_lab_support <- "P:/toolboxes/lab_support/"

# load libraries & source files ------------------
library(tidyverse)

# create jobs tibble ---------------
for (i in algorithm) {
  if (i == "glmnet") { 
    jobs_tmp <- expand_grid(n_repeat = NA_integer_,
                      n_fold = NA_integer_,
                      algorithm = "glmnet",
                      feature_set,
                      hp1 = hp1_glmnet,
                      hp2 = NA_integer_,
                      hp3 = NA_integer_,
                      resample,
                      cv_type)
    } else if (i == "random_forest") {
    jobs_tmp <- expand_grid(n_repeat = 1:as.numeric(str_split(str_remove(cv_type, "_x"), "_")[[1]][2]),
                        n_fold = 1:as.numeric(str_split(str_remove(cv_type, "_x"), "_")[[1]][3]),
                        algorithm = "random_forest",
                        feature_set,
                        hp1 = hp1_rf,
                        hp2 = hp2_rf,
                        hp3 = hp3_rf,
                        resample,
                        cv_type)
    } else if (i == "knn") {
    jobs_tmp <- expand_grid(n_repeat = 1:as.numeric(str_split(str_remove(cv_type, "_x"), "_")[[1]][2]),
                            n_fold = 1:as.numeric(str_split(str_remove(cv_type, "_x"), "_")[[1]][3]),
                            algorithm = "knn",
                            feature_set,
                            hp1 = hp1_knn,
                            hp2 = NA_integer_,
                            hp3 = NA_integer_,
                            resample,
                            cv_type)      
    }
  
  # bind jobs files
  jobs <- if (i == algorithm[1])
    jobs_tmp
  else
    rbind(jobs, jobs_tmp)
}

# add job num to file --------------- 
jobs <- jobs %>% 
  rownames_to_column("job_num") 

# create new job directory (if it does not already exist) -------------------
if (!dir.exists(file.path(path_jobs, name_job))) {
  dir.create(file.path(path_jobs, name_job))
  dir.create(file.path(path_jobs, name_job, "input"))
  dir.create(file.path(path_jobs, name_job, "output"))
} else {
  stop("Job folder already exists. No new folders created.")
}

# write jobs file to input folder ---------------
jobs %>% 
  write_csv(file.path(path_jobs, name_job, "input", "jobs.csv"))

# copy data to input folder as data_trn -----------------
file.copy(from = file.path(path_data, data_trn),
          to = file.path(path_jobs, name_job, "input/data_trn.csv")) %>% 
  invisible()

# copy template R files to input folder -----------------
file.copy(from = file.path(path_templates, "input", c(list.files(file.path(path_templates, "input")))),
          to = file.path(path_jobs, name_job, "input"),
          recursive = TRUE) %>% 
  invisible()

# copy chtc functions to input folder
file.copy(from = file.path(path_lab_support, "chtc.R"),
          to = file.path(path_jobs, name_job, "input"),
          recursive = TRUE) %>% 
  invisible()

# copy template aggregate script to output folder ---------------
file.copy(from = file.path(path_templates, "post_chtc_processing.Rmd"),
          to = file.path(path_jobs, name_job, "output/post_chtc_processing.Rmd")) %>% 
  invisible()

# update queue on submit file -----------------
queue <- str_c("queue ", nrow(jobs))
write(queue, file.path(path_jobs, name_job, "input/sub_meta.sub"), append = TRUE)
