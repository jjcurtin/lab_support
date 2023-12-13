# Functions to support data cleaning and tabular EDA.  They are used extensively in PSY752 (Intro to Applied Machine Learning) but also by our lab.

# Author:  John Curtin (jjcurtin@wisc.edu)

# Provide summary statistics for cleaning EDA
skim_some <- skimr::skim_with(numeric = skimr::sfl(mean = NULL, sd = NULL, p25 = NULL, p50 = NULL, p75 = NULL, hist = NULL))

# Provides summary statistics for modeling EDA
skew_na <- purrr::partial(e1071::skewness, na.rm = TRUE)
kurt_na <- purrr::partial(e1071::kurtosis, na.rm = TRUE)
skim_all <- skimr::skim_with(numeric = skimr::sfl(skew = skew_na, kurtosis = kurt_na, hist = NULL),
                             factor = skimr::sfl(ordered = NULL))


# provides simple table with counts and proportions
tab <- function(df, var, sort = FALSE) {
  df |>  dplyr::count({{ var }}, sort = sort) |> 
    dplyr::mutate(prop = n / sum(n))
} 

# provides simple table with counts for cross tab
# uses janitor::tabyl but included to have two tab functions
tab2 <- function(df, var1, var2) {
  df |>  janitor::tabyl({{ var1 }}, {{ var2 }})
} 


# Somewhat unformatted printing of text responses for categorical variables.
# Used primarily to confirm that responses are valid and tidy
# print_responses <- function(name, column){
#   unique(column) |>
#     na.omit() |>
#     stringr::str_c(collapse = ", ") |>
#     stringr::str_c(name, ": ", ., "\n") |>
#     cat()
# }

# Used to tidy the responses/levels/labels for factors
tidy_responses <- function(column){
  # replace all non-alphanumeric with _
  column <- fct_relabel(column, \(column) str_replace_all(column, "\\W", "_"))
  # replace whitespace with _
  column <- fct_relabel(column, \(column) str_replace_all(column, "\\s+", "_"))
  # replace multiple _ with single _
  column <- fct_relabel(column, \(column) str_replace_all(column, "\\_+", "_"))
  #remove _ at end of string
  column <- fct_relabel(column, \(column) str_replace_all(column, "\\_$", ""))
  # remove _ at start of string
  column <- fct_relabel(column, \(column) str_replace_all(column, "\\^_", ""))
  # convert to lowercase
  column <- fct_relabel(column, tolower)
  factor(column)
}

print_kbl <- function(data, height = "500px", align = "r", digits = 2, caption = NULL){
  data |>
    kableExtra::kbl(align = align, digits = digits, caption = caption) |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "condensed")) |>
    kableExtra::scroll_box(height = height, width = "100%")
}
