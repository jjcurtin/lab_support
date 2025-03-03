var_score <- function(d, forward_items, reverse_items = NULL, item_range = NULL, prorate = TRUE, max_miss = .20)
{

  # select relevant items
  d <- d |> 
    dplyr::select(dplyr::all_of(forward_items), dplyr::all_of(reverse_items))

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
        d <- d |> dplyr::mutate({{v}} := (sum(item_range) - .data[[v]]))
    }   
  }

  max_missing_cols <- ncol(d) * max_miss
  d |> 
    dplyr::rowwise() |> 
    dplyr::mutate(total = dplyr::if_else(prorate,
                           mean(dplyr::c_across(dplyr::everything()), na.rm = TRUE) * ncol(d), # if true
                           sum(dplyr::c_across(dplyr::everything()), na.rm = TRUE))) |>       # if false
    dplyr::mutate(missing_cols = sum(is.na(dplyr::c_across(!dplyr::contains("total"))))) |> # count miss cols excluding total
    dplyr::mutate(total = dplyr::if_else(missing_cols > max_missing_cols,  # set total to NA if too many missing
                           NA_real_,
                           total)) |> 
    dplyr::ungroup() |> 
    dplyr::pull(total)
}


# Demo
# RECREATE DEMO ON BASE R DATASET
# 
# library(ds4psy)
# # Sample dataset with item scores
# d <- posPsy_AHI_CESD
# forward_items <- c("ahi01","ahi02","ahi03", "ahi04", "ahi05")
# reverse_items <- c("ahi06","ahi07","ahi08", "ahi09", "ahi10")
# item_range <- c(1,5)
# 
# 
# a <- d |> dplyr::mutate(ahi_score = var_score(_, c("ahi01","ahi02","ahi03", "ahi04", "ahi05"), c("ahi06","ahi07","ahi08", "ahi09", "ahi10"), c(1,5)))
# glimpse(a)