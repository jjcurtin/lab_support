# fit model at chtc 

# libraries & source functions file ----------------
suppressPackageStartupMessages({
  require(dplyr)
  require(readr)
  require(tidyr)
  require(stringr)
})
source("chtc.R")

# set up job_num ---------------
# process_num <- 1
args <- commandArgs(trailingOnly = TRUE) 
process_num <- as.numeric(args[1]) + 1 # process/job arg starts at 0

# read in jobs.csv file ------------------
jobs <- read_csv("jobs.csv", col_types = cols()) 

# pull out job ------------------
job <- slice(jobs, process_num)

# read in data train --------------- 
d <- read_csv("data_trn.csv", col_types = cols())

# create splits object ---------------
set.seed(102030)
splits <- make_splits(d = d, job = job)

# build recipe ----------------
rec <- build_recipe(d = d, job = job)

# fit model and get predictions and metrics ----------------
results <- tune_model(job = job, rec = rec, folds = splits)

# write out results tibble ------------
file_name <- str_c("results_", process_num, ".csv")
results %>% 
  write_csv(., file_name)
