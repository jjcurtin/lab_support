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


check_area_code <- function(number) {
  # This function can be called inside extract_number to perform a
  # check that area code is a valid US code. It returns a boolean 
  # value. This function will help validate area codes that are
  # known US codes vs out of country codes with similar lengths.
  
  # initialize vector of US area codes
   
  # TEMPORARILY READING IN DB - this will be hardcoded as vector
  codes <- read_csv("Z:/studydata/risk/analysis/meta/notes/area_code_db.csv", col_types = cols()) %>% 
    glimpse()
  us_codes <- codes %>% 
    filter(country == "US") %>% 
    select(area_code) %>% 
    unlist(use.names = FALSE)
  
  us_codes
  
  # hard code vector of area codes
  us_codes <- c(201, 202, 203, 205, 206, 207, 208, 209, 210, 212, 213, 214, 215, 216, 217, 218,
                219, 220, 223, 224, 225, 227, 228, 229, 231, 234, 239, 240, 248, 251, 252, 253,
                254, 256, 260, 262, 267, 269, 270, 272, 274, 276, 279, 281, 283, 301, 302, 303,
                304, 305, 307, 308, 309, 310, 312, 313, 314, 315, 316, 317, 318, 319, 320, 321,
                323, 325, 326, 327, 330, 331, 332, 334, 336, 337, 339, 340, 341, 346, 347, 351,
                352, 353, 360, 361, 364, 369, 380, 385, 386, 401, 402, 404, 405, 406, 407, 408,
                409, 410, 412, 413, 414, 415, 417, 419, 423, 424, 425, 430, 432, 434, 435, 440,
                442, 443, 445, 447, 448, 458, 463, 464, 469, 470, 475, 478, 479, 480, 484, 501,
                502, 503, 504, 505, 507, 508, 509, 510, 512, 513, 515, 516, 517, 518, 520, 526,
                530, 531, 534, 539, 540, 541, 551, 557, 559, 561, 562, 563, 564, 567, 570, 571,
                572, 573, 574, 575, 580, 582, 585, 586, 601, 602, 603, 605, 606, 607, 608, 609,
                610, 612, 614, 615, 616, 617, 618, 619, 620, 623, 626, 627, 628, 629, 630, 631,
                636, 640, 641, 646, 650, 651, 656, 657, 659, 660, 661, 662, 667, 669, 670, 671,
                678, 679, 680, 681, 682, 684, 689, 701, 702, 703, 704, 706, 707, 708, 710, 712,
                713, 714, 715, 716, 717, 718, 719, 720, 724, 725, 726, 727, 730, 731, 732, 734,
                737, 740, 743, 747, 754, 757, 760, 762, 763, 764, 765, 769, 770, 771, 772, 773,
                774, 775, 779, 781, 785, 786, 787, 801, 802, 803, 804, 805, 806, 808, 810, 812,
                813, 814, 815, 816, 817, 818, 820, 826, 828, 830, 831, 832, 835, 838, 839, 840,
                843, 845, 847, 848, 850, 854, 856, 857, 858, 859, 860, 862, 863, 864, 865, 870,
                872, 878, 901, 903, 904, 906, 907, 908, 909, 910, 912, 913, 914, 915, 916, 917,
                918, 919, 920, 925, 928, 929, 930, 931, 934, 935, 936, 937, 938, 939, 940, 941,
                943, 945, 947, 948, 949, 951, 952, 954, 956, 959, 970, 971, 972, 973, 975, 978,
                979, 980, 984, 985, 986, 989)
  
  # pull first 3 digits of number
  code <- str_sub(1, 3)
  
  # check if area code is in vector of known US area codes
  # return TRUE or FALSE
  for (i in seq_along(us_codes)) {
    matches <- str_detect(code, us_codes[i])
    if(length(matches) == 0) {  
      return(FALSE)
    }
    if(length(matches) == 1) {
      return(TRUE)
    }
    if(length(matches) > 1) {
      # Need to error out here since shouldn't match more than one area code.
    }
  }
}


extract_number <- function(number) {
  # takes a number string, strips off the country code and any other
  # formatting to return a single series of digits (still as character)
  # Numbers not formatted will be returned as "unknown" and will need to 
  # be checked further.
  
  
  # Can use function with following code:
  # logs$address_clean <- map(logs$address, kendra_cln_number)
  # logs <- logs %>% 
  #   mutate(address_clean = unlist(address_clean)) %>% 
  #   glimpse()
  
  # using separate varaible to store formatted numbers so that numbers
  # not caught by any of the cases are returned as "unknown" for further
  # EDA/inspection.
  number_formatted <- NULL
 
  if(!is.na(number)) {
    # exclude numbers with alphabetic characters from cleaning and
    # return "unknown"
    if(str_detect(number, "[[:alpha:]]")) {
      return("unknown")
    }
    
    # Remove spaces, parentheses, and dashes 
    if(str_detect(number, "[[:space:]-\\(\\)]")) {
      number_formatted <- str_remove_all(number, "[[:space:]-\\(\\)]")
    }
  
    # Remove +1 from US numbers
    # check area codes don't start with a 0 or 1 in filter
    if(str_detect(number, "^\\+1[2-9]") & nchar(number) == 12  & is.null(number_formatted)) {
      number_formatted <- str_remove(number, "^\\+1")
    }
    # check formatted numbers
    if (!is.null(number_formatted)) {
      if(str_detect(number_formatted, "^\\+1[2-9]") & nchar(number_formatted) == 12) {
        number_formatted <- str_remove(number_formatted, "^\\+1", "")
      } 
    }
  
    # remove 1 from US numbers with no +
    if(str_detect(number, "^1[2-9]") & nchar(number) == 11 & is.null(number_formatted)) {
      number_formatted <- str_remove(number, "^1")
    }
    # check formatted numbers
    if (!is.null(number_formatted)) {
      if(str_detect(number_formatted, "^1[2-9]") & nchar(number_formatted) == 11) {
        number_formatted <- str_remove(number_formatted, "^1")
      } 
    }
  
    # remove + from US numbers (with no 1 or +1) 
    if(str_detect(number, "^\\+[2-9]") & nchar(number) == 11 & is.null(number_formatted)) {
       number_formatted <- str_remove(number, "^\\+")
    }
    # check formatted numbers
    if (!is.null(number_formatted)) {
      if(str_detect(number_formatted, "^\\+[2-9]") & nchar(number_formatted) == 11) {
        number_formatted <- str_remove(number_formatted,  "^\\+")
      } 
    }
    
    # move all numbers already in proper format to formatted_numbers variable
    if(str_detect(number, "^[2-9]") & nchar(number == 10)) {
      number_formatted <- number
    }
    
    if(!is.null(number_formatted)) { 
      return(number_formatted)
      # return all unformatted numbers as "unknown"
    } else { return("unknown")}
  }
  
  return(as.character(NA))
 
}


