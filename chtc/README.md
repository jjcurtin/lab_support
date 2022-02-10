# CHTC Read Me

## Study level files 
`training_controls.R` is a study level file that should be copied over to a project's repo and edited as necessary. It contains a study specific recipe, job-specific parameters, chtc parameters (e.g., memory, disk space), and global attributes (outcome variable and cv_type). **This file CAN be renamed if you are using different controls for a single study. It is automatically renamed to `training_controls.R` in `mak_jobs` when copied over to the chtc input folder so it can still be sourced automatically in the `fit_chtc` script.**    

`mak_jobs.R` is in the CHTC folder and is generalized to make CHTC jobs based on the `training_controls.R` script. You should execute `mak_jobs.R` from any R project in a local repo. You will only need to change the path for where your study-specific `training_controls.R` file is located. For this script to work you should also ensure you have an updated local copy of lab_support and/or you are on the correct branch if you are in the dev process. 

## Static project level files
There are a set of template files in `static_files` that will be automatically copied over to your project path and do not need further editing. These include files in the input folder: chtc scripts for submitting, executing, and processing your jobs, a function script and a fit script that will be executed for each job.  And files for processing data in output folder. Note that the `post_chtc_processing.Rmd` script in the output folder is broken into 2 parts. This will be aggregated into a single script when you call `mak_jobs.R`. The reason it is is in two pieces is so that the correct source location of the study specific training controls can automatically be added to the Rmd script. As a result the Rmd script can be run in its entirety without any editing. 

## After running Make Jobs
Once `mak_jobs` is run, you should have the following at the jobs file path you specified in `training_controls.R`.
1. Input folder containing all files that need to be transferred over to the submit server - `execute.sh`, `fit_chtc.R`, `fun_chtc.R`, `training_controls.R`, `pre.sh`, `post.sh`, `sub.sub`, `data_trn.csv`, and `jobs.csv`
2. In the output folder you will have the post processing script `post_chtc_processing.Rmd` which will unzip, aggregate and summarize model performance and plot hyperparameters. This output folder should also be the destination location for the 3 zip files you will drag back over from CHTC - `error.zip`, `output.zip`, and `results.zip`   

## Tars
- We will have a lab chtc tar file with the essential packages for running scripts in lab_support/chtc/tars. (This is not there yet. I think our closest version to this currently is risk.tar.gz on the server in risk/chtc/tars - it looks like JC started to break this apart into features and training so I am not clear on which one to copy over to lab_support).  
- We will supplement this with a study specific tar as needed. All tars used should be specified in training controls so that the submit file knows what to transfer, but you will need to manually drag any tars you are using over to CHTC.   
- CHTC has good documentation for building tars at https://chtc.cs.wisc.edu/uw-research-computing/r-jobs.html#build    
- You will need to use a build script to add packages to an existing tar or create a new tar. There is a build script in lab_support/chtc/tars titled build_tar.sub   