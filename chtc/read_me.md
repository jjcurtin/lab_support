# CHTC Read Me

## Study level files 
`training_controls.R` is a study level file that should be coppied over to a project's repo and edited as necessary. It contains a study specific recipe, job-specific parameters, and global attributes (outcome variable and cv_type). This file should NOT be renamed since it is sourced automatically in the `fit_chtc` and `mak_jobs` scripts.  

## Project level files
`mak_jobs.R` is in the CHTC folder and is generalized to make CHTC jobs based on the `training_controls.R` script. You will need to execute `mak_jobs.R` from any R project in a local repo. You will also need to change the path for where your study-specific `training_controls.R` file is located. There are also a set of files in templates that will be automatically copied over to your project path and do not need further editing. The function script `fun_chtc.R` will also automatically be copied over.  

## CHTC
Once `mak_jobs` is run, you should have the following at the jobs file path you specified in `mak_jobs`.
1. Input folder containing all files that need to be transferred over to the submit server - `execute.sh`, `fit_chtc.R`, `fun_chtc.R`, `training_controls.R`, `pre.sh`, `post.sh`, `sub.sub`, `data_trn.csv`, and `jobs.csv`
2. In the output folder you will have the post processing script `post_chtc_processing.Rmd` which will unzip, aggregate and summarize model performance and plot hyperparameters. This output folder should be the destination location for the 3 zip files returned by CHTC `error.zip`, `output.zip`, and `results.zip`