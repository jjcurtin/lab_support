# Description-----------------------------------------

# Useful online resources
#   For android SMS" https://synctech.com.au/sms-backup-restore/fields-in-xml-backup-files/
#   https://developer.android.com/reference/android/provider/CallLog - There are a few different sections (left-hand menu) relating to calls and SMS


# Packages------------------------------------------
library(stringr)


# Functions --------------------------------------
check_country_code <- function(number) {
  # Function checks if a number is a valid international (non-US) number.
  # Number is expected to be a character string with no symbols
  # Returns true or false
  
  # Check number is in proper format
  if (is.na(number)) {
    stop("No number detected (NA).")
  }
  
  if (!str_detect(number, "[0-9]")) {
    stop("You entered the number ", number,
         ". This function expects a formatted number with no text characters.")
  }
  
  if (str_detect(number, "[[:space:]-\\(\\).+]")) {
    stop("You entered the number ", number,
         ". This function expects a formatted number with no punctuation.")
  }
 
  match <- NULL
  
  #  Mexico
  #  Country code 52
  #  pattern - 10 digit number plus country code 52 and has valid area code
  if (nchar(number) == 12 && str_detect(number, "^52") && !str_detect(number, "[[:alpha:]*#]")) {
    # check area code - code can be 2 or 3 digits
    code_2_digits <- str_sub(number, 3, 4) %in% c ("55", "56", "81", "33") 
    code_3_digits <- str_sub(number, 3, 5) %in% c("656", "614", "618", "999", "990",
                                                  "221", "222", "442", "446", "449",
                                                  "663", "664", "612", "624", "844",
                                                  "686", "667", "722", "729", "998",
                                                  "871", "744", "444", "440", "833",
                                                  "477", "479", "961", "662", "663", 
                                                  "645", "644", "642", "631", "229",
                                                  "443", "921", "771", "981", "899",
                                                  "868")
    
    if(code_2_digits | code_3_digits) {
      if (is.null(match)) {
        match <- TRUE
      } else stop(number, " matches multiple pre-defined patterns")
    }
  }
  
  ## Africa
  
  ## Asia
  #  India
  #  Country code 91
  #  pattern - 10 digits plus country code 91 with valid area code
  if (nchar(number) == 12 && str_detect(number, "^91") && !str_detect(number, "[[:alpha:]*#]")) {
    # check area code - code can be 2, 3, or 4 digits
    # FIX: these area codes are for landlines, there are no specific area codes on mobile phones
    code_2_digits <- str_sub(number, 3, 4) %in% c ("79", "80", "44", "11", "40", "33", "22",
                                                   "11", "20") 
    code_3_digits <- str_sub(number, 3, 5) %in% c("080", "562", "145", "144", "183", "265",
                                                  "755", "151", "747", "172", "422", "135",
                                                  "141", "181", "512", "744", "522", "821",
                                                  "612", "145", "281", "144", "177", "194",
                                                  "261", "294", "265")
    code_4_digits <- str_sub(number, 3, 6) %in% c("2432", "2982", "1462", "5644", "1482", 
                                                  "8572", "6432", "5642", "5619", "8252",
                                                  "2974", "1362", "1582", "7462")
    
    # if(code_2_digits | code_3_digits | code_4_digits) {
      if (is.null(match)) {
        match <- TRUE
      } else stop(number, " matches multiple pre-defined patterns")
    # }
  }
  
  #  Cambodia
  #  Country code 855
  #  pattern - 8 or 9 digit phone number plus 855 country code and with valid area code
  if ((nchar(number) == 12 | nchar(number) == 11) && str_detect(number, "^855") && 
      !str_detect(number, "[[:alpha:]*#]")) {
    # check mobile phone provider access code
    check_code <- str_sub(number, 4, 5) %in% c("23", "24", "25", "26", "32", "33", "34",
                                               "35", "36", "42", "43", "44", "52", "53",
                                               "54", "55", "62", "63", "64", "65", "72",
                                               "73", "74", "75", "11", "12", "14", "17",
                                               "61", "76", "77", "78", "79", "85", "89", 
                                               "92", "95", "99", "38", "39", "18", "31",
                                               "60", "66", "67", "68", "71", "88", "90", 
                                               "97", "13", "80", "83", "84", "10", "15", 
                                               "16", "69", "70", "81", "86", "87", "93",
                                               "96", "98")
    
    if(check_code) {
      if (is.null(match)) {
        match <- TRUE
      } else stop(number, " matches multiple pre-defined patterns")
    }
  }
  
  ## Oceania
  
  ## Europe
  #  UK
  #  Country code 44
  #  pattern - 9 or 10 numbers plus country code 44 
  #  all UK mobile phones are 10 digits, but landlines may be 9 or 10 digits
  if (str_detect(number, "^44") && (nchar(number) == 12 | nchar(number) == 11) && 
      !str_detect(number, "[[:alpha:]*#]")) {
    # area codes are 2, 3, 4, or 5, digits in length. 
    # Geographic area codes start with a 1 or 2, mobile phones start with 7
    # can also start with 3, 5, or 8 though
    # length of number and country code is likely sufficient check for now
    
    if(is.null(match)) {
      match <- TRUE
    } else stop(number, " matches multiple pre-defined patterns")
  }
  
  #  Iran
  #  Country code 98
  #  pattern - 10 numbers plus country code 98 and valid area code
  if (str_detect(number, "^98") && nchar(number) == 12 && 
      !str_detect(number, "[[:alpha:]*#]")) {
    # check area code
    check_code <- (str_sub(number(2, 3) %in% c("11", "13", "17", "21", "23", "24", "25", 
                                               "26", "28", "31", "34", "35", "38", "41",
                                               "44", "45", "51", "54", "56", "58", "61",
                                               "66", "71", "74", "76", "77", "81", "83",
                                               "84", "86", "87")))
    
    if(check_code) {
      if (is.null(match)) {
        match <- TRUE
      } else stop(number, " matches multiple pre-defined patterns")
    }
  }
  
  # Russia
  # Country code 7
  # pattern 10 numbers plus country code 7 and valid area code
  if (nchar(number) == 11 && str_detect(number, "^7") && !str_detect(number, "[[:alpha:]*#]")) {
    # check mobile phone provider access code
    check_code <- (str_sub(number, 2, 4) %in% c("877", "385", "818", "885", "851", "892",
                                                "472", "483", "301", "351", "820", "302",
                                                "835", "821", "872", "343", "895", "873",
                                                "395", "493", "401", "847", "484", "814",
                                                "384", "390", "833", "494", "861", "391",
                                                "352", "471", "813", "474", "836", "370",
                                                "834", "495", "496", "815", "855", "893",
                                                "891", "831", "816", "383", "381", "486",
                                                "353", "841", "811", "879", "863", "491",
                                                "846", "845", "867", "481", "862", "812",
                                                "865", "475", "843", "848", "382", "487",
                                                "349", "394", "482", "345", "341", "842",
                                                "492", "844", "817", "473", "485", "884")) |
      # non-geographic area codes for mobile networks (900 - 969, except 954, and 972-999)
      (as.numeric(str_sub(number, 2, 4)) >= 900 && as.numeric(str_sub(number, 2, 4)) <= 999 && 
         !as.numeric(str_sub(number, 2, 4)) %in% c(954, 970, 971))
    
    if(check_code) {
      if (is.null(match)) {
        match <- TRUE
      } else stop(number, " matches multiple pre-defined patterns")
    }
  }
  
  #  Ireland
  #  Country Code 353
  #  pattern - 7-9 numbers plus country code 353
  if ((nchar(number) >= 10 && nchar(number) <= 12) && str_detect(number, "^353") &&
      !str_detect(number, "[[:alpha:]*#]")) {
    # area codes are different and confusing need to come back and add them
    if(is.null(match)) {
      match <- TRUE
    } else stop(number, " matches multiple pre-defined patterns")
  }
  
  #  France
  #  Country code 33
  #  pattern - 9 digit phone number plus country code 33
  if (nchar(number) == 11 && str_detect(number, "^33") && !str_detect(number, "[[:alpha:]*#]")) {
    # add area code check 
    if(is.null(match)) {
      match <- TRUE
    } else stop(number, " matches multiple pre-defined patterns")
  }
  
  #  Germany
  #  Country code 49
  #  pattern - 10 - 11 digit number plus country code 49
  #  FIX: this pattern is for landlines only - "There used to be no fixed lengths for either area codes 
  #  or subscriber telephone numbers, meaning that some subscriber numbers may be as short as two digits."
  
  if (str_detect(number, "^49") && !str_detect(number, "[[:alpha:]*#]")) {
    # area codes very complex, will add here 
    # https://en.wikipedia.org/wiki/List_of_dialling_codes_in_Germany#017
    if(is.null(match)) {
      match <- TRUE
    } else stop(number, " matches multiple pre-defined patterns")
  }
  
  #  Norway
  #  Country code 47
  #  pattern - 8 digit number plus country code 47
  if (nchar(number) == 8 && str_detect(number, "^47") && 
      !str_detect(number, "[[:alpha:]*#]")) {
   # 1-2 digit area codes plus mobile codes to add in 
    if(is.null(match)) {
      match <- TRUE
    } else stop(number, " matches multiple pre-defined patterns")
  }
  
  #  Belgium
  #  Country code 32
  #  pattern - 9 digit number with country code 32 and valid area code
  if (nchar(number) == 11 && str_detect(number, "^32") && !str_detect(number, "[[:alpha:]*#]")) {
    # check area code - code can be 1 or 2 digits
    code_1_digit <- str_sub(number, 3, 3) %in% c ("3", "2", "9", "4") 
    code_2_digits <- str_sub(number, 3, 4) %in% c("53", "63", "68", "65", "19", "50", "71",
                                                  "60", "83", "52", "13", "82", "69", "86",
                                                  "89", "11", "85", "57", "56", "64", "16",
                                                  "61", "84", "15", "81", "67", "54", "59",
                                                  "51", "55", "80", "12", "14", "87", "58",
                                                  "10")
    
    if(code_1_digit | code_2_digits) {
      if(is.null(match)) {
        match <- TRUE
      } else stop(number, " matches multiple pre-defined patterns")
    }
  }
  
  #  Switzerland
  #  Country code 41
  #  pattern - 9 digit number plus country code 41 and with valid area code
  if ((nchar(number) == 11 | nchar(number) == 12) && str_detect(number, "^41") && 
      !str_detect(number, "[[:alpha:]*#]")) {
    # check area code
    code_2_digits <- str_sub(number, 3, 4) %in% c("21", "22", "24", "26", "27", "31", "32",
                                                  "33", "34", "41", "43", "44", "51", "52",
                                                  "55", "56", "58", "61", "62", "71", "74",
                                                  "75", "76", "77", "78", "79", "81", "91")
    code_3_digits <- str_sub(number, 3, 4) %in% c("800", "840", "842", "844", "848")
    
    if(code_2_digits | code_3_digits) {
      if(is.null(match)) {
        match <- TRUE
      } else stop(number, " matches multiple pre-defined patterns")
    }
  }
  
  ## Central America
  
  ## South America
  #  Columbia
  #  Country code 57
  #  pattern - 10 digit number plus country code 57 and has valid mobile phone access code
  if (nchar(number) == 12 && str_detect(number, "^57") && !str_detect(number, "[[:alpha:]*#]")) {
    # check mobile phone provider access code
    check_code <- str_sub(number, 3, 5) %in% c("300", "301", "302", "304", "305", "310", 
                                               "311", "312", "313", "314", "320", "321", 
                                               "322", "323", "315", "316", "317", "318",
                                               "319", "324", "350", "351")
    
    if(check_code) {
      if(is.null(match)) {
        match <- TRUE
      } else stop(number, " matches multiple pre-defined patterns")
    }
  }
  
  if (is.null(match)) {
    return(FALSE)
  } else return(match)
}




extract_country_code <- function(number){
# Takes a number string and extracts the country code (if exists)
# or returns the US country code if no country code was provided

}



check_area_code <- function(number) {
  # number is expected to be a 10 character string.  Function checks if the
  # the first three characters of this number represent a valid US area code or
  # North American toll free area code.
  # Returns true or false

  # only need to check NA once
  if (str_length(number) != 10 | is.na(number)) {
    stop("You entered the number ", number,
         ". This function expects a formatted number with 10 digits.")
  }

  if (str_detect(number, "\\+")) {
    stop("You entered the number ", number,
         ". This function expects a formatted number with no leading +.")
  }

  if (str_length(number) == 11 && str_detect(number, "^1")) {
    stop("You entered the number ", number,
         ". This function expects a formatted number with no country code.")
  }

  if (str_detect(number, "[[:alpha:]]")) {
    stop("You entered the number ", number,
         ". This function expects a formatted number with no text characters.")
  }

  if (str_detect(number, "[[:punct:]]")) {
    stop("You entered the number ", number,
         ". This function expects a formatted number with no punctuation.")
  }

  # initialize vector of US area codes
  # full db of area codes at https://nationalnanpa.com/reports/reports_npa.html
  # area_code_db.csv and area_code_codebook.xls in lab_support/data
  
  us_codes <-
    as.character(
      c(201, 202, 203, 205, 206, 207, 208, 209, 210, 212, 213, 214, 215, 216, 217, 218,
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
        979, 980, 984, 985, 986, 989,
        # North American toll free area codes
        800, 833, 844, 855, 866, 877, 888,
        # Canadian area codes
        587, 780, 825, 403, 250, 604, 236, 778, 204, 431, 506, 709, 867, 249, 343, 416, 
        519, 647, 905, 365, 548, 705, 226, 289, 613, 807, 437, 902, 782, 438, 418, 450, 
        367, 579, 873, 514, 581, 819, 306, 639,
        # United States territories (Puerto Rico, Guam, etc.)
        340, 670, 671, 684, 787, 939,
        # Carribean area codes (from Carribean nations with +1 international code)
        # https://en.wikipedia.org/wiki/List_of_country_calling_codes#Zone_1:_North_American_Numbering_Plan
        242, 246, 264, 268, 284, 345, 441, 473, 649, 658, 876, 664, 721, 758, 767, 784, 
        809, 829, 849, 868, 869,
        # Personal communication services
        500, 521, 522, 523, 524, 533, 544, 566, 577, 588))

  code <- str_sub(number, 1, 3)

  return(code %in% us_codes)
}




extract_number <- function(number, print_warning = FALSE) {
  # Takes a number string, strips off the country code and any other
  # formatting to return a single series of digits (still as character)

  # Numbers that match multiple patterns will generate error.
  # Numbers that do not match any pattern are returned as is (but without
  # spaces, dashes, and ()) 
  
  # To print warnings for unmatched numbers set print_warning = TRUE

  # Can use function in tidy pipeline with following code:
  # logs <- logs %>% mutate(clean_numbers = purrr::map_chr(numbers, extract_number))

  orig_number <- number  # used when no pattern match to retain unformatted form

  # Pattern - NA
  # return now to avoid need to check NA each time
  if (is.na(number)) return(NA_character_)

  # Pattern - characters
  # e.g., email address, amber alert
  # checked and returned before removing spaces, dashes, etc

  # pattern - email.  characters & @ and "." in the suffix after @
  if (str_detect(number, "^[[:alnum:]._-]+@[[:alnum:].-]+.[:alpha:]$")) {
    return(number)
  }
  
  # pattern - names or numbers with no numbers
  if (str_detect(number, "[[:alpha:]]") && !str_detect(number, "[0-9]")) {
    return(number)
  }
  

  # pattern - amber alert/commercial mobile alert system - relevant only for SMS
  # (?i) is case-insensitive modifier
  if (str_detect(number, "#CMAS") || str_detect(number, "(?i)alert")) {
    return(number)
  }

  # Now format before checking all other patterns
  # Remove spaces, parentheses, and dashes
  if(str_detect(number, "[[:space:]-\\(\\).+]")) {

    number <- str_remove_all(number, "[[:space:]-\\(\\).+]")
  }
  
  # remove invisible unicode left to right character
  # special case seen in IOS
  if(str_detect(number, "^\u200E")) {
    number <- str_remove(number, "\u200E")
  } 

  # will copy number to formatted number to allow detection of multiple pattern matches.
  # Not needed yet but may be when numbers can match both US & Non-US numbers
  formatted_number <- NULL
  
  # Remove 00 standard prefix for international dialing (from outside US)
  # HANDLE - think through if this affects anything else
  if (str_detect(number, "^00")) {
    number <- str_remove(number, "00")
  }
  
  # Remove 011 prefix needed for international dialing in North America
  if (str_detect(number, "^011")) {
    number <- str_remove(number, "011")
  }
  # May also show as 11
  if (str_detect(number, "^11")) {
    number <- str_remove(number, "11")
  }
  
  # US numbers - removes US country code and returns 10 digit number
  # Pattern - US numbers with 1 country code
  if (nchar(number) == 11 && str_detect(number, "^1") && !str_detect(number, "[[:alpha:]*#]")
      && check_area_code(str_sub(number, 2, 11))) {

    if(is.null(formatted_number)) {
      formatted_number <- str_remove(number, "1")
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }

  # Pattern - 10 digit US numbers with valid area code
  if (nchar(number) == 10 && !str_detect(number, "[[:alpha:]*#]") && check_area_code(number)) {

    if(is.null(formatted_number)) {
      formatted_number <- number
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }
  
  # pattern - *67 plus 10 digit US phone number - blocks number
  if (nchar(number) == 13 && str_detect(number, "\\*67") && check_area_code(str_sub(number, 4, 13))) {
    if(is.null(formatted_number)) {
      formatted_number <- str_remove(number, "\\*67")
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }
  
  # pattern - *67 plus 10 digit US number plus country code 1
  if (nchar(number) == 14 && str_detect(number, "\\*671") && check_area_code(str_sub(number, 5, 14))) {
    if(is.na(formatted_number)) {
      formatted_number <- str_remove(number, "\\*671")
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }
  

  # pattern - N11 numbers (https://en.wikipedia.org/wiki/N11_code)
  # 211: Community services and information
  # 311: Municipal government services, non-emergency number
  # 411: Directory assistance
  # 511: Traffic information or police non-emergency services
  # 611: Telephone company (telco) customer service and repair
  # 711: TDD and Relay Services for the deaf and hard of hearing
  # 811: Underground public utility location
  # 911: Emergency services (police, fire, ambulance and rescue services)
  if (nchar(number) == 3 && str_detect(number, "[2-9]1{2}")) {
    if(is.null(formatted_number)) {
      formatted_number <- number
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }

  # pattern - 988 (National suicide hotline)
  if (number == "988") {
    if(is.null(formatted_number)) {
      formatted_number <- number
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }

  # pattern - vertical service codes (https://en.wikipedia.org/wiki/Vertical_service_code)
  # JOHN - many of these may not be relevant for cell phones, I am only checking
  # for ones that seem likely to occur, but we can use the resource above if
  # we see more slipping through.
  if(number %in% c("*69", "*86", "*61", "*63", "*66", "*70", "*82")) {
    if(is.null(formatted_number)) {
      formatted_number <- number
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }

  # pattern - short codes.  5-6 digits, first digit is 2 or greater
  # https://en.wikipedia.org/wiki/Short_code#United_States
  if ((nchar(number) == 5 && str_detect(number, "[2-9][0-9]{4}")) ||
      (nchar(number) == 6 && str_detect(number, "[2-9][0-9]{5}"))) {
    if(is.null(formatted_number)) {
      formatted_number <- number
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }

  # pattern - 800050001020.  Likely Verizon Wireless Automated number
  if (number == "800050001020") {
    if(is.null(formatted_number)) {
      formatted_number <- number
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }

  # pattern - *22899, *228, *611
  # possible service numbers for Verizon
  if (number == "*22899" | number == "*228" | number == "*611") {
    if(is.null(formatted_number)) {
      formatted_number <- number
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }

  # pattern - 7726
  # FTC reporting number for SPAM
  if (number == "7726") {
    if(is.null(formatted_number)) {
      formatted_number <- number
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }

  # pattern - string with IBTU, JBTU, or KBTU
  # Indicates IOS calls made with facebook messenger
  if (str_detect(number, "IBTU") | str_detect(number, "JBTU") | str_detect(number, "KBTU")) {
    if (is.na(formatted_number)) {
      formatted_number <- number
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }

  
  # HANDLE - group messages
  # These show up in my android logs as multiple numbers separated by ~
  # I think these are in IOS data as being separated by ;
  

  # HANDLE - Check non-US country codes
  if (check_country_code(number)) {
    if(is.null(formatted_number)) {
      formatted_number <- number
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }
    
  # Pattern - 7 digit US numbers with no area code
  # This may eventually interact with non-US numbers?
  # Can we get a list of all know US exchanges?
  if (nchar(number) == 7) {
    if(is.null(formatted_number)) {
      formatted_number <- number
    } else {
      stop(number, " matches multiple pre-defined patterns")
    }
  }

  if (is.null(formatted_number)) {
    formatted_number <- orig_number
    # generate warning if number did not match any format
    if (print_warning) warning (orig_number, " did not match any pre-defined pattern")
  }

  return(formatted_number)
}




