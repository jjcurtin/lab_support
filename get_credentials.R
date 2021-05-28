library(readr)
library(dplyr)

get_credentials <- function(resourcename, filename){
# Function assumes there is a securely stored csv file (full file name and path
# specified by "file_name") # that includes a column named 
# "resource_name" and additional columns with names that match the credentials 
# associated with that resource (e.g., username, password, key).
# Returns a list with these credentials to use in other functions
  
  credentials <- read_csv(filename) %>% 
    filter(resource == resourcename)
  
  return(credentials)
}