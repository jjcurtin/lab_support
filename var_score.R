var_score <- function(d, forward_items, reverse_items = NULL, item_range = NULL, prorate = TRUE, max_miss = .20)
{
  library(dplyr)

  # select relevant items
  d <- d %>% 
    select(all_of(forward_items), all_of(reverse_items))

  # check for out of range
  if (!is.null(item_range)) {
    if (!((min(d, na.rm = TRUE) %in% item_range || max(d, na.rm = TRUE) %in% item_range))) {
      stop("Item score(s) out of range")
    }
  }
  
  # check that length of range == 2 if reverse is not null
  if (!is.null(reverse_items) && length(item_range) != 2) {
    stop("Must specify item range (range) to reverse score items")
  }

  # reverse score relevant items
  if (!is.null(reverse_items)) {
    for (v in reverse_items) {                  
        d <- d %>% mutate({{v}} := (sum(item_range) - .data[[v]]))
    }   
  }

  max_missing_cols <- ncol(d) * max_miss
  d <- d %>% 
    rowwise() %>% 
    mutate(total = if_else(prorate,
                           mean(c_across(everything()), na.rm = TRUE) * ncol(.), # if true
                           sum(c_across(everything()), na.rm = TRUE))) %>%       # if false
    mutate(missing_cols = sum(is.na(c_across(!contains("total"))))) %>% # count miss cols excluding total
    mutate(total = if_else(missing_cols > max_missing_cols,  # set total to NA if too many missing
                           NA_real_,
                           total)) %>% 
    ungroup()
  return(d$total)
}


# Demo
# RECREATE DEMO ON BASE R DATASET
# 
# library(ds4psy)
# library(tidyverse)
# library(lmSupport)
# # Sample dataset with item scores
# d <- posPsy_AHI_CESD
# forward_items <- c("ahi01","ahi02","ahi03", "ahi04", "ahi05")
# reverse_items <- c("ahi06","ahi07","ahi08", "ahi09", "ahi10")
# item_range <- c(1,5)
# 
# 
# # New var_score
# a <- d %>% mutate(ahi_score = var_score(.,c("ahi01","ahi02","ahi03", "ahi04", "ahi05"), c("ahi06","ahi07","ahi08", "ahi09", "ahi10"), c(1,5)))
# glimpse(a)
# 
# # Old varScore
# b <- a %>% mutate(ahi_score = varScore(.,c("ahi01","ahi02","ahi03", "ahi04", "ahi05"), c("ahi06","ahi07","ahi08", "ahi09", "ahi10"), c(1,5)))
# glimpse(b)
# 
# # results are equal
# all(a$ahi_score == b$ahi_score)
