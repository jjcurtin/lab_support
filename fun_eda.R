# Functions to support data cleaning and tabular EDA.  They are used extensively in Psy 752 (Intro to Applied Machine Learning) but also by our lab.

# Author:  John Curtin (jjcurtin@wisc.edu)

library(tidyverse)
library(skimr) # for use of skim_with()

# Provide summary statistics for cleaning EDA
skim_some <- skim_with(numeric = sfl(mean = NULL, sd = NULL, p25 = NULL, p50 = NULL, p75 = NULL, hist = NULL))

# Provides summary statistics for modeling EDA
skew_na <- partial(e1071::skewness, na.rm = TRUE)
kurt_na <- partial(e1071::kurtosis, na.rm = TRUE)
skim_all <- skim_with(numeric = sfl(skew = skew_na, kurtosis = kurt_na))


# provides simple table with counts and proportions
tab <- function(df, var, sort = FALSE) {
  df %>% count({{ var }}, sort = sort) %>% 
    mutate(prop = n / sum(n))
}


# Somewhat unformatted printing of text responses for categorical variables.
# Used primarily to confirm that responses are valid and tidy
print_responses <- function(name, column){
  unique(column) %>%
    na.omit() %>%
    str_c(collapse = ", ") %>%
    str_c(name, ": ", ., "\n") %>%
    cat()
}

# Used to tidy the responses/levels/labels for categorical variables
tidy_responses <- function(column){
  column <- factor(column)
  levels(column) <- janitor::make_clean_names(levels(column), use_make_names = FALSE) 
  as.character(column)
}