# Required packages -------------------------------------------------------
library(stringr)    #str_c
library(tidyverse)  #bind_rows(), tibble()
library(jsonlite)   #fromJSON()
library(httr)       #GET (for API access to FollowMee)

#Function: api_followmee_gps----------------------------------------

api_followmee_gps <- function(device_id,from,to){
  url <- 'http://www.followmee.com'
  path <- '/api/tracks.aspx'
  
  api_key <- '12cf03011635a89978f641cf8b54441c'
  user_name <- 'ARCRISK1'
  output_type <- 'json'
  fn = 'daterangefordevice'
  
  #sample data
  #from = '2018-07-10'
  #to = '2018-07-11'
  #device_id = 11964044
  
  query <-  str_c('?key=', api_key,
                  '&username=', user_name,
                  '&output=', output_type,
                  '&function=', fn,
                  '&from=',from,
                  '&to=',to,
                  '&deviceid=',device_id)
  
  get_url <- str_c(url, path, query)
  
  j_gps <-  GET(url = get_url)
  
  if(http_type(j_gps) != "application/json") {
    stop('GPS not formated at JSON')
  }
  
  d_gps <- fromJSON(content(j_gps, "text"), simplifyVector = TRUE) %>% 
    .$Data
  return(d_gps)
}

#Function:  api_followmee_devices--------------------------------------------

api_followmee_devices <- function(){
  url  <-  'http://www.followmee.com'
  path <- '/api/info.aspx'
  
  api_key <- '12cf03011635a89978f641cf8b54441c'
  user_name <- 'ARCRISK1'
  output_type <- 'json'
  fn <- 'devicelist'
  
  query <- str_c('?key=', api_key,
                '&username=', user_name,
                '&output=',output_type,
                '&function=', fn)
  get_url <- str_c(url, path, query)
  
  j_devices <- GET(url=get_url)
  
  if(http_type(j_devices) != "application/json") {
    stop('Devices not formated at JSON')
  }
  
  d_devices <-  fromJSON(content(j_devices, "text"), simplifyVector = TRUE) %>% 
    .$Data
  
  return(d_devices)
}

