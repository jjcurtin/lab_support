# Required packages
library(tidyverse)  # bind_rows()
library(jsonlite)   # fromJSON()
library(httr)       # GET()
library(lubridate)  # days(), as_datetime()


get_followmee_devices <- function(creds) {
# Returns a table of all devices associated with account
# Use get_credentials() in lab_support to obtain creds
  
  url  <-  "http://www.followmee.com"
  path <- "/api/info.aspx"
  
  output_type <- "json"
  fn <- "devicelist"
  
  query <- str_c("?key=", creds$key,
                "&username=", creds$username,
                "&output=",output_type,
                "&function=", fn) %>% 
    str_c(url, path, .)
  
  response <- GET(url = query)
  
  if (!response$status_code == 200) {
    stop("FollowMee API GET Status Code: ", response$status_code)
  }
  
  devices <-  fromJSON(content(response, "text"), simplifyVector = TRUE) %>% 
    .$Data
  
  return(devices)
}


get_followmee_deviceid <- function(subid, creds) {
# Returns a device id for a specific subid
# Use get_credentials() in lab_support to obtain creds
  
  if (is.numeric(subid)) {
    subid <- as.character(subid)
  }

  device_id <- get_followmee_devices(creds) %>% 
    filter(DeviceName == subid) %>% 
    pull(DeviceID)
  
  return(device_id)
}

get_followmee_data <- function(subid, creds, n_days = 7) {
# Gets up to past 14 days (n_days) of location data. 
# Use get_credentials() in lab_support to obtain creds.
  
  date_end <- Sys.Date()
  date_start <- date_end - days(n_days - 1)
  device_id <- get_followmee_deviceid(subid, creds)
  
  url <- "http://www.followmee.com"
  path <- "/api/tracks.aspx"
  output_type <- "json"
  fn = "daterangefordevice"
  
  query <-  str_c("?key=", creds$key,
                  "&username=", creds$username,
                  "&output=", output_type,
                  "&function=", fn,
                  "&from=", date_start,
                  "&to=", date_end,
                  "&deviceid=", device_id) %>% 
    str_c(url, path, .)
  
  response <- GET(url = query)
  
  if (! (response$status_code == 200)) {
    stop("FollowMee API GET Status Code: ", response$status_code)
  }
  
  data <- fromJSON(content(response, "text"), simplifyVector = TRUE) %>% 
    .$Data %>% 
    as_tibble() %>% 
    rename(date_chr = Date, lat = Latitude, lon = Longitude, type = Type,
           speed_mph = `Speed(mph)`, direction = Direction,
           altitude_ft = `Altitude(ft)`, accuracy = Accuracy) %>% 
    select(-`Speed(km/h)`, -`Altitude(m)`) %>% 
    mutate(date = as_datetime(date_chr),
           subid = subid) %>% 
    relocate(subid, date) %>% 
    relocate(date_chr, .after = last_col())
  
  return(data)
}

update_followmee_data <- function(past_data, creds) {
# Binds up to past 14 days of new location data to an existing tibble of 
# geolocation data (past_data) for a subid.  Removes redundant/duplicated points 
# and sorts on date.  Use get_credentials() in lab_support to obtain creds.

  subid <- past_data$subid[[1]]
  
  data <- get_followmee_data(subid = subid, creds = creds, n_days = 7) %>% 
    bind_rows(past_data %>% mutate(date_chr = as.character(date_chr))) %>% 
    distinct(subid, date, lat, lon, .keep_all = TRUE) %>% 
    arrange(date)
}