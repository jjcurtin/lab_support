## Coding Style & Standards
- **Preferred Style:** Follow the [tidyverse style guide](https://tidyverse.org). Use `snake_case` for variable and function names.
- **Pipe Operator:** Use the native pipe `|>` (R 4.1+) whenever possible

## Technical Preferences
- **Data Manipulation:** Prioritize `dplyr`, `tidyr`, and `purrr`.
- **Visualization:** Use `ggplot2` for all plots. Default to `theme_minimal()` unless otherwise specified.
- **Modeling:** Use the `tidymodels` ecosystem for machine learning tasks.
- Use the `here::here()` function when you need to create reproducible file paths
- Generally prioritize the `tidyverse` and `tidymodels` ecosystem
- If you use functions from packages outside of base R and the `tidyverse` and `tidymodels` ecosystems, include their namespace in the function call
- In the hopefully rare instances that you choose to use a function from base R or another package over an existing `tidyverse` or `tidymodels` function, provide a comment to justify that choice
* I typically work with `.R` and `.qmd` (Quarto) files for coding and reports
* I typically save data in the comma-separated values (with `.csv` ) format

## Safety
* Never suggest code that deletes files (`unlink`, `file.remove`) unless explicitly asked
* Only save/write data files to `~/tmp` unless explicitly asked
* Only save  `.R` and `.qmd` (Quarto) files to folders within the current project unless explicitly asked
* Never save/write over an existing file unless explicitly asked
* Always ask before you save/write files
