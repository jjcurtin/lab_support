library(tidyverse)
library(tidymodels)

source("https://github.com/jjcurtin/lab_support/blob/main/format_path.R?raw=true")

path <- format_path("studydata/risk/chtc/messages/train_glmnet_nested_1_x_10_3_x_10_v2_meta_stratified/input")

source(here::here(path, "training_controls.R"))
source(here::here(path, "fun_chtc.R"))

d <- read_csv(here::here(path, "data_trn.csv"),
              show_col_types = FALSE)

d <- format_data(d)  

glimpse(d)


# Create nested outer splits object
splits <- d |> 
  make_splits(cv_resample_type, cv_resample, cv_outer_resample, 
              cv_inner_resample, cv_group, cv_strat = stratify,
              the_seed = seed_splits)


splits


# Outer tests------
# get train/test for outer loop fold 1
out1_train <- training(splits$splits[[1]]) 

out1_test <- testing(splits$splits[[1]])

# Check sample sizes reflect 10-fold
nrow(d)/10 # 1 fold of 10-fold should be roughly 1203
nrow(out1_train) # train is good (9/10)
nrow(out1_test) # test is good (1/10)

# check subids are grouped 
out1_train |> 
  filter(subid %in% out1_test) # no subids in test in training

# stratify porportions
out1_train |> 
  janitor::tabyl(any_lapse)

out1_test |> 
  janitor::tabyl(any_lapse)

# Inner tests------
# get inner splits for outer loop fold 1
out1_inners <- splits$inner_resamples[[1]] 

# get inner train/test fold 1 for outer fold 1
out1_inner1_train <- training(out1_inners$splits[[1]]) 
out1_inner1_test <- testing(out1_inners$splits[[1]]) 

# Check sample sizes reflect 10-fold
nrow(out1_train)/10 # 1 fold of 10-fold should be roughly 1077
nrow(out1_inner1_train) # train is good (9/10)
nrow(out1_inner1_test) # test is good (1/10)

# check subids are grouped 
out1_inner1_train |> 
  filter(subid %in% out1_inner1_test) # no subids in test in training

# stratify porportions
out1_inner1_train |> 
  janitor::tabyl(any_lapse)

out1_inner1_test |> 
  janitor::tabyl(any_lapse)
