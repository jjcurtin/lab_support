#!/bin/bash
# post.sh
# JJC - will be replaced by a script that transfers raw (not zipped files)
# For windows, this is a winscp script

# move data to results
mv results_*.csv results

#move error files to error
# mv error*.err error

# move output files to output
# mv output*.out output

# put files to transfer into zip files
zip -r -m results results
zip -r -m error error
zip -r -m output output

# move input files into input_files
mkdir input_files
mv p*.sh data_trn.rds jobs.csv f*.R sub.sub execute.sh input_files

