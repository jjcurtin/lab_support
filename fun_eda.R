# Functions to support data cleaning and tabular EDA.  They are used extensively in PSY752 (Intro to Applied Machine Learning) but also by our lab.

# Author:  John Curtin (jjcurtin@wisc.edu)

# Provide summary statistics for cleaning EDA
skim_some <- skimr::skim_with(numeric = skimr::sfl(mean = NULL, sd = NULL, p25 = NULL, p50 = NULL, p75 = NULL, hist = NULL))

# Provides summary statistics for modeling EDA
skew_na <- partial(e1071::skewness, na.rm = TRUE)
kurt_na <- partial(e1071::kurtosis, na.rm = TRUE)
skim_all <- skimr::skim_with(numeric = skimr::sfl(skew = skew_na, kurtosis = kurt_na, hist = NULL))


# provides simple table with counts and proportions
tab <- function(df, var, sort = FALSE) {
  df |>  dplyr::count({{ var }}, sort = sort) |> 
    dplyr::mutate(prop = n / sum(n))
} 


# Somewhat unformatted printing of text responses for categorical variables.
# Used primarily to confirm that responses are valid and tidy
print_responses <- function(name, column){
  unique(column) |>
    na.omit() |>
    stringr::str_c(collapse = ", ") |>
    stringr::str_c(name, ": ", ., "\n") |>
    cat()
}

# Used to tidy the responses/levels/labels for categorical variables
tidy_responses <- function(column){
  column <- factor(column)
  levels(column) <- janitor::make_clean_names(levels(column), use_make_names = FALSE) 
  as.character(column)
}

print_kbl <- function(data, height = "500px", align = "r", digits = 2, caption = NULL){
  data |>
    kableExtra::kbl(align = align, digits = digits, caption = caption) |>
    kableExtra::kable_styling(bootstrap_options = c("striped", "condensed")) |>
    kableExtra::scroll_box(height = height, width = "100%")
}
