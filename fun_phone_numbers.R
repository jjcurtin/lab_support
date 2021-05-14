# Description:



# Packages
library(stringr)




clean_numbers <- function(raw_numbers){
  # does appropriate processing of sms or voice call numbers
  # SHOULD BE UPDATED TO WORK ON A SINGLE NUMBER RATHER THAN VECTOR BY JOHN
  # NEEDS UPDATING BY KENDRA OR JOHN
  # WILL NOT HANDLE NON US COUNTRY CODES

  numbers <- raw_numbers %>%
    str_remove("^1") %>% # 16082176221
    str_remove("^\\+1") %>%   # US country codes
    str_remove_all("[\\(\\) ]") %>%
    str_remove_all("-")

  # if(any(str_detect(numbers, "^\\+"))) {
  #   stop("Unprocessed country code detected")
  # }

  # numbers <- if_else(str_detect(numbers, "^\\([0-9]{3}\\) "),
  #                   str_replace_all(numbers, "[\\(\\) ]", ""),
  #                   numbers)
  return(numbers)
}

format_numbers <- function(numbers){
  # formats a simple number character string something easier
  # to read for participant.
  # SHOULD EVENTUALLY BE UPDATED FOR NON-US NUMBERS AND TO WORK ON SINGLE NUMBER
  # RATHER THAN VECTOR
  # SHOULD CHECK FOR COUNTRY CODE AND NUMBER LENGTH EVENTUALLY AS NEEDED

  # if 10 digit US number
  formatted_numbers <- str_c("(",
                             str_sub(numbers, 1, 3),
                             ") ",
                             str_sub(numbers, 4, 6),
                             "-",
                             str_sub(numbers, 7, 10))

  return(formatted_numbers)
}
