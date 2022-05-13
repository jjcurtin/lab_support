#!/bin/bash
# execute.sh

# uncomment code below and edit username and file extension if transferring large file from staging directory - file should already be in staging 
# Using a file transfer application, like WinSCP, directly drag the large file from its location on your computer to a location within /staging/username/ on transfer.chtc.wisc.edu.
# IMPORTANT: Also be sure to remove large file name from submit script (under transfer files)
# IMPORTANT: Your file will need to start with data_trn to be read in correctly in fit script. Remember your file automatically gets renamed to this when mak_jobs is run.
# Therefore it is recommended you make your jobs and then drag the renamed data_trn file to the transfer server on WinSCP
# cp /staging/username/data_trn.rds.xz ./

#untar R installation
tar -xzf R402.tar.gz
tar -xzf SLIBS.tar.gz
tar -xzf train.tar.gz
export LD_LIBRARY_PATH=$(pwd)/SS:$LD_LIBRARY_PATH

#use that R installation
export PATH=$(pwd)/R/bin:$PATH
export RHOME=$(pwd)/R
export R_LIBS=$PWD/packages

# Commands to enable modules, and then load an appropriate module (necessary to load glmnet and xgboost)
export PATH ./etc/profile.d/modules.sh 
module load GCC/8.3.0

#run R script, passing in args
Rscript fit_chtc.R $1

# uncomment if using large file from staging directory to remove file from woriking directory
# May need to edit extension on large file (i.e., .csv.xz)
# rm data_trn.rds.xz