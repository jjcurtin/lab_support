# Description:

# Useful online resources
#   For android SMS" https://synctech.com.au/sms-backup-restore/fields-in-xml-backup-files/
#   https://developer.android.com/reference/android/provider/CallLog - There are a few different sections (left-hand menu) relating to calls and SMS


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


extract_countrycode <- function(number){
# Takes a number string and extracts the country code (if exists)
# or returns the US country code if no country code was provided

}


extract_number <- function(number){
# takes a number string, strips off the country code and any other
# formatting to return a single series of digits (still as character)

}




kendra_cln_number <- function(number) {
# I am thinking we should return the clean numbers as a new variable and then
# we can easily see which numbers were not caught/formatted by looking at
# missing values in formatted column.
  
# Can use function with following code:
  # logs$address_clean <- map(logs$address, kendra_cln_number)
  # logs <- logs %>% 
  #   mutate(address_clean = unlist(address_clean)) %>% 
  #   glimpse()
  
  number_formatted <- NULL
 
  if(!is.na(number)) {
    # Remove spaces, parentheses, and dashes 
    # exlude numbers with alphabetic characters from cleaning spaces and dashes
    if(str_detect(number, "[[:space:]-\\(\\)]") & !str_detect(number, "[[:alpha:]]")) {
      number_formatted <- str_replace_all(number, "[[:space:]-\\(\\)]", "")
    }
  
    # Remove +1 from US numbers
    # check area codes don't start with a 0 or 1 in filter
    if(str_detect(number, "^\\+1[2-9]") & nchar(number) == 12  & is.null(number_formatted)) {
      number_formatted <- str_replace(number, "^\\+1", "")
    }
    # check formatted numbers
    if (!is.null(number_formatted)) {
      if(str_detect(number_formatted, "^\\+1[2-9]") & nchar(number_formatted) == 12) {
        number_formatted <- str_replace(number_formatted, "^\\+1", "")
      } 
    }
  
    # remove 1 from US numbers with no +
    if(str_detect(number, "^1[2-9]") & nchar(number) == 11 & is.null(number_formatted)) {
      number_formatted <- str_replace(number, "^1", "")
    }
    # check formatted numbers
    if (!is.null(number_formatted)) {
      if(str_detect(number_formatted, "^1[2-9]") & nchar(number_formatted) == 11) {
        number_formatted <- str_replace(number_formatted, "^1", "")
      } 
    }
  
    # remove + from US numbers (with no 1 or +1) 
    if(str_detect(number, "^\\+[2-9]") & nchar(number) == 11 & is.null(number_formatted)) {
       number_formatted <- str_replace(number, "^\\+", "")
    }
    # check formatted numbers
    if (!is.null(number_formatted)) {
      if(str_detect(number_formatted, "^\\+[2-9]") & nchar(number_formatted) == 11) {
        number_formatted <- str_replace(number_formatted,  "^\\+", "")
      } 
    }
    
    if(!is.null(number_formatted)) {
      return(number_formatted)
    }
  }
  
  return(as.character(NA))
 
}
