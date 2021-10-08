# CHTC Read Me

## Study level files 
`training_controls.R` is a study level file that should be copied over to a project's repo and edited as necessary. It contains a study specific recipe, job-specific parameters, and global attributes (outcome variable and cv_type). **This file should NOT be renamed since it is sourced automatically in the `fit_chtc` and `mak_jobs` scripts.**  

## Project level files
`mak_jobs.R` is in the CHTC folder and is generalized to make CHTC jobs based on the `training_controls.R` script. You should execute `mak_jobs.R` from any R project in a local repo (No need to copy this over since you will only need to change the path for where your study-specific `training_controls.R` file is located). For this script to work you should also ensure you have an updated local copy of lab_support and/or you are on the correct branch if you are in the dev process. 

There are also a set of template files in `static_files` that will be automatically copied over to your project path and do not need further editing. Note that the `post_chtc_processing.Rmd` script in the output folder is broken into 2 parts. This will be aggregated into a single script when you call `mak_jobs.R`. The reason it is is in two pieces is so that the correct source location of the study specific training controls can automatically be added to the Rmd script. As a result the Rmd script can be run in its entirety without any editing. 

The function script `fun_chtc.R` will also automatically be copied over and does not need further editing.  

## CHTC
Once `mak_jobs` is run, you should have the following at the jobs file path you specified in `training_controls.R`.
1. Input folder containing all files that need to be transferred over to the submit server - `execute.sh`, `fit_chtc.R`, `fun_chtc.R`, `training_controls.R`, `pre.sh`, `post.sh`, `sub.sub`, `data_trn.csv`, and `jobs.csv`
2. In the output folder you will have the post processing script `post_chtc_processing.Rmd` which will unzip, aggregate and summarize model performance and plot hyperparameters. This output folder should also be the destination location for the 3 zip files you will drag back over from CHTC - `error.zip`, `output.zip`, and `results.zip`