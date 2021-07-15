# Prints a dataframe or other table-like object with nice formatting
# Defaults to a output box for height = "500px"
# Might want to use height = "100%" if only printing a few rows

print_kbl <- function(data, height = "500px"){
  data %>%
    kableExtra::kbl(align = "r") %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "condensed")) %>%
    kableExtra::scroll_box(height = height, width = "100%")
}
