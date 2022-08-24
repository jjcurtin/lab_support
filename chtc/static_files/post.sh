#!/bin/bash
# post.sh
# JJC - will be replaced by a script that transfers raw (not zipped files)
# For windows, this is a winscp script
# KW - my files still seem to take a long time to transfer when I don't zip so I may continue to use this script (I need to experiment a little more).

# move data to results
mv results_*.csv results


# OPTIONAL - zip folders
# zip -r -m results results
# zip -r -m error error

# move input files into input_files folder to clean up server 
mkdir input_files
mv p*.sh data_trn.rds jobs.csv f*.R sub.sub execute.sh input_files

