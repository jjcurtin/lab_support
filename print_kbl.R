# Prints a dataframe or other table-like object with nice formatting
# Defaults to a output box for height = "500px"
# Might want to use height = "100%" if only printing a few rows
# Set align to "l" if printing large blocks of text in kable table
# this makes the text more readable by adjusting the alignment

print_kbl <- function(data, height = "500px", align = "r"){
  data %>%
    kableExtra::kbl(align = align) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "condensed")) %>%
    kableExtra::scroll_box(height = height, width = "100%")
}
