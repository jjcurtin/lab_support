# Name of function: `composite_score_long`
# Author: Zihan Li (zli2545@wisc.edu, https://github.com/zli2545)
# Create Date: 2024-08

# Introduction

# This function is part of the workflow in opioid relapse risk prediction project in Addiction Research Center of UW-Madison
# (https://arc.psych.wisc.edu)

# Basically, there are 3 columns in the df would be considered and operated in this function:
# (1) id
# (2) variable name
# (3) variable value

# Description

# This function is designed to calculate composite score of a set of items in a long-format dataframe. See examples below for more details.
# The logic of the function is straightforward: select all needed variable columns and items and change the reverse-coded items' scores.
# The whole dataframe will be filtered for the interested variables and then piped to correct the scores of reverse-coded items.
# Composite score of each participant is calculated by summarizing and inserted back to the long-format data with binding.

# Details of the arguments are listed at the end of this file.
# The function works in a `tidy` flow, which may require the `tidyverse` and `dplyr` packages

# Function

composite_score_long <- function(df, id, item_col, value_col, forward=NULL, reverse=NULL, range_lo=NULL, range_up=NULL, max_miss = 0.25, comp_item_name = "comp_score") {

  # Internal function to reverse scores
  reverse_code <- function(x, range_lo, range_up) {
    return(range_lo + range_up - x)
  }

  # ---- General data ----
  # All items combined from forward and reverse coded items
  items <- c(forward, reverse)
  # Extract the values in the `item_col` column
  item_col_vars <- df |>
    pull({{ item_col }}) |>
    unique()
  # Extract the values in the `value_col` column
  value_col_vals <- df |>
    filter({{ item_col }} %in% items) |>
    pull({{ value_col }}) |>
    as.numeric() |>
    unique()

  # ---- Arguments Check ----
  # Types of items; reverse could be NULL, if there is no reverse-coded items
  if (is.null(forward)) {
    stop("Error: Missing at least the list of forward/-coded items - `forward`.")
  }

  # Range of values
  if (is.null(range_lo) | is.null(range_up)) {
    stop("Error: Both `range_lo` and `range_up` must be provided.")
  }

  if (!all(items %in% item_col_vars)) {
    stop("Error: Some items specified in `forward` or `reverse` do not exist in the dataframe.")
  }
  # Check for invalid values out of range
  if (any(value_col_vals < range_lo | value_col_vals > range_up, na.rm = TRUE)) {
    stop("Error: Some values in `value_col` are out of the provided range.")
  }


  # ---- Creating composite scores ----
  df_comp <- df |>
    # select variables to calculate the composite
    filter({{ item_col }} %in% items) |>
    # convert CLASS to be calculable
    mutate({{ value_col }} := as.numeric({{ value_col }})) |>
    # change the reverse-coded scores to be forward
    mutate({{ value_col }} := if_else({{ item_col }} %in% reverse,
                                      reverse_code({{ value_col }}, range_lo, range_up),
                                      {{ value_col }})) |>
    # break down by each participant
    group_by({{ id }}) |>
    # summation and count of missing value
    summarise(
      item_c = sum({{ value_col }}, na.rm = TRUE),
      na_count = sum(is.na({{ value_col }})),
      count = n()
    ) |>
    # calculate missing rate to prorate or substitute with NA
    mutate(miss_ratio = na_count / count,
           item_c_pro = if_else(miss_ratio < max_miss, item_c / (1 - miss_ratio), NA)) |>
    # convert back CLASS
    mutate(item_c_pro = as.character(item_c_pro)) |>
    # insert back to original df
    select({{ id }}, item_c_pro) |>
    mutate({{ item_col }} := comp_item_name) |>
    rename({{ value_col }} := item_c_pro) |>
    bind_rows(df) |>
    # reorder
    arrange({{ id }}, {{ item_col }})

  return(df_comp)
}


# Arguments

# df: Dataframe to operate, containing at least 3 columns (id, item_col, value_col).
#
# id: Column containing the id info, or any other label related to the items correspondingly.
#
# item_col: Column containing the item/variable names.
#
# value_col: Column containing the value/answer for the item/variables.
#
# forward: List containing the forward-coded items, elements should be quoted, e.g. c('item_1', 'item_2').
#
# reverse: List containing the reverse-coded items, elements should be quoted, e.g. c('item_1', 'item_2').
#
# range_lo: Lower range of the possible values for the items, numeric, e.g. 1, 4.
#
# range_up: Upper range of the possible values for the items, numeric.
#
# max_miss: Maximum of the tolerable ratio of missing values. If the missing ratio is larger than this, the composite score will be assigned as `NA`.
#
# comp_item_name: Name assigned to the newly-created composite score item, a string, e.g. 'composite_score'.

# Example

# A detailed example showing how the function works could be found in:
# https://neuspace.gitbook.io/posts_neuki/v/functions-of-r/composite_score_long_dsm

