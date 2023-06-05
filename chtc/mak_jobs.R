# setup chtc jobs & associated files/folders

# NOTE: this script should be run from a project on the repo (e.g., RISK, MATCH, RISK2) 
# for the relative paths to access local copy of lab_support to work

# CHANGE ME  -------------------- 
# Set path for training_controls.R
# location of study specific training_controls.R (can use relative path if in repo) 
path_training_controls <- "./PROJECT/chtc/training_controls.R" 


# DON'T CHANGE -------------------- 
suppressPackageStartupMessages(library(tidyverse))
source("../lab_support/chtc/fun_make_jobs.R", echo = FALSE)

# set overwrite_jobs to FALSE if you do not want to overwrite existing files
make_jobs(path_training_controls, overwrite_jobs = TRUE)