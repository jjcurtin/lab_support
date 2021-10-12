#!/bin/bash
# post.sh

# move data to results
mv results_*.csv results

# move model predictions to predictions
mv preds_*.rds predictions

 #move error files to error
mv error*.err error

# move output files to output
mv output*.out output

# put files to transfer into zip files
zip -r -m results results
zip -r -m predictions predictions
zip -r -m error error
zip -r -m output output

# move input files into input_files
mkdir input_files
mv p*.sh data_trn.csv jobs.csv f*.R sub.sub execute.sh input_files

