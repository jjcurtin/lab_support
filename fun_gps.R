# Functions to support working with location data

# Required packages
library(leaflet) # leaflet() and other related functions
library(geosphere)   # distGeo(), distmean()
library(stringr)
library(httr)
library(tidyr)
library(dplyr)

plot_places <- function(places, labels = NULL) {
# places is a tibble that has two required and one optional column:
# lat (numeric), lon (numeric), and info(character).
# Other columns are ignored.
# see https://rstudio.github.io/leaflet/ for more info on leaflet

  if (is.null(labels)) {
    places$.labels <-as.character(1:nrow(places))
  } else places[".labels"] <- places[labels]

  map <-  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = places,
                     lng = ~lon, lat = ~lat,
                     radius = 1.5, color = "red", opacity = 1,
                     popup = ~.labels) %>%
    addMeasure(position = "bottomleft",
               primaryLengthUnit = "meters",
               primaryAreaUnit = "sqmeters")

  return(map)
}


plot_tracks <- function(locs, gap = 2, overlay_points = TRUE) {
# locs is df with lat and lon at a minimum
# gap is max mins between points before track is broken into different segments
# overlay_points allows for points to be plotted on top of tracks
# see https://rstudio.github.io/leaflet/ for more info on leaflet
  locs <- locs %>%
    arrange(time) %>%
    mutate(time_next = difftime(lead(time), time, units = "mins"),
           label = as.character(row_number()))

  map <-  leaflet() %>%
    addTiles() # Add default OpenStreetMap map tiles

    # more options and use of groups
    # addTiles(group = 'Color') %>% # Add default OpenStreetMap map tiles
    # addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = 'B&W') #B&W OSM tiles

  i <- 1
  while(i < nrow(locs)) { # need at least two points (current and next) for a new track
    track <- tibble(lat = double(), lon = double())  # make new empty track

    track <- track %>%
      add_row(lat = locs$lat[i], lon = locs$lon[i])

    while(i < nrow(locs) &&
          !is.na(locs$time_next[i]) &&
          locs$time_next[i] <= minutes(gap)) {
      i <- i + 1
      track <- track %>%
        add_row(lat = locs$lat[i], lon = locs$lon[i])
    }


    map <- map %>%
      addPolylines(data = track,
                      lng = ~lon, lat = ~lat,
                      color = "red", weight = 2,
                      opacity = 1) # , group = 'tracks'

    i <- i + 1  # advance to next trackpoint (first in next track)
  }

  # use of controls for overlaps and measurement
  # map <- map %>%
  #   addLayersControl(baseGroups = c('color', 'B&W'),
  #                    overlayGroups = c('places', 'tracks'),
  #                    options = layersControlOptions(collapsed = FALSE)) %>%
  #   addMeasure(position = "bottomleft",
  #              primaryLengthUnit = "meters",
  #              primaryAreaUnit = "sqmeters")

  if (overlay_points) {
    map <- map %>%
      addCircleMarkers(data = locs,
                     lng = ~lon, lat = ~lat,
                     radius = .1, color = "blue", opacity = .5,
                     popup = ~label)
  }

  map <-  map %>%
    addMeasure(position = "bottomleft",
               primaryLengthUnit = "meters",
               primaryAreaUnit = "sqmeters")

  return(map)
}



count_places <- function(places, max_dist = 50) {
  places$visits <- 0

  for (i in 1:nrow(places)) {
    for (j in 1:nrow(places)) {
      dist <- distGeo(c(places$lon[i], places$lat[i]), c(places$lon[j], places$lat[j]))
      if (dist <= max_dist) places$visits[i] <- places$visits[i] + 1
    }
  }
  return(places)
}


geomean_places <- function(places, max_dist = 50){
# Takes a tibble of places from a single subject and groups them sequentially into
# into groups that differ by no more than max_dist meters
# from the previous points in the group.  It then returns
# a weighted mean of the the location for each group
# It assumes columns named subid, date, lat, lon, cnt_pts, duration

  places <- places %>%
    mutate(duration = as.numeric(duration))
  
  if (nrow(places) > 1) {
    places <- mutate(places, place_grp = NA)
    max_grp <- 0

    for(i in 1:(nrow(places))){

      if (is.na(places$place_grp[i])) {
        max_grp <- max_grp + 1
        current_grp <- max_grp
        places$place_grp[i] <- current_grp
      } else {
        current_grp <- places$place_grp[i]
      }

      # check i place against later rows if exist
      if (i < nrow(places)) {
        for (j in (i + 1):nrow(places)){
          if(distGeo(c(places$lon[i], places$lat[i]),
                     c(places$lon[j], places$lat[j])) <= max_dist){
            places$place_grp[j] <- current_grp
          }
        }
      }
    }

    avg_place <- tibble(date = Date(), lat = double(), lon = double(), cnt_pts = double(), duration = double())
    for (i in 1:max_grp) {
      places_grouped <- places %>%
        dplyr::filter(place_grp == i)

      if (nrow(places_grouped) > 1) {
        xy <- places_grouped %>%
          select(lon, lat)
        w <- places_grouped$cnt_pts
        xy_new <- geomean(xy, w)
        avg_place <- avg_place %>%
          add_row(date = max(places_grouped$date),  # most recent date
                  lat = xy_new[1,2],
                  lon = xy_new[1,1],
                  cnt_pts = sum(places_grouped$cnt_pts),
                  duration = sum(places_grouped$duration))
                            
      } else {
        avg_place <- avg_place %>%
          add_row(select(places_grouped, date, lat, lon, cnt_pts, duration))
      } 
    }
    
    # add subid
    avg_place <- avg_place %>%
      mutate(subid = places$subid[[1]]) %>%
      relocate(subid)

  } else  avg_place <- places %>% select(subid, date, lat, lon, cnt_pts, duration)
 
  return(avg_place)
}


geomean_seq_pts <- function(locations, max_dist = 50) {
# Use with single subid at a time

# https://www.gps.gov/systems/gps/performance/accuracy/
# https://geoawesomeness.com/how-accurate-is-your-smartphones-gps-in-an-urban-jungle/#:~:text=The%20results%20showed%20that%20an,as%20669%20feet%20(204m).
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0219890
# https://www.androidcentral.com/location-services-whats-difference-between-choices-and-which-should-i-pick

  locations <- locations %>%
    mutate(cnt_pts = 1,
           time_next = difftime(lead(time), time, units = "mins"),
           lon_next = dplyr::lead(lon),
           lat_next = dplyr::lead(lat),
           lon_prev = dplyr::lag(lon),
           lat_prev = dplyr::lag(lat)) %>%
    rowwise() %>%
    mutate(dist_next = distGeo(c(lon, lat),   # in meters
                               c(lon_next, lat_next)),
           dist_prev = distGeo(c(lon, lat),
                               c(lon_prev, lat_prev))) %>%
    ungroup()


  row_i <- 1

  agg_previous <- FALSE
  agg_next <- FALSE
  skip <- FALSE

  while (row_i < nrow(locations)) { # iterate through each row, except the last

    message(str_c("Checking row ", row_i, " of ", nrow(locations)))

    # evaluate the row as:
    # point to be aggregated with previous row
    if (row_i != 1 &&
        locations$dist_prev[row_i] < locations$dist_next[row_i] &&
        locations$dist_prev[row_i] <= max_dist) {
      agg_previous <- TRUE
      message("...aggregating row with PREVIOUS")
    }

    # point to be aggregated with next row
    if (row_i != 1 &&
        locations$dist_next[row_i] <= locations$dist_prev[row_i] &&
        locations$dist_next[row_i] <= max_dist) {
      agg_next <- TRUE
      message("...aggregating row with NEXT")
    }

    # neither
    if (!agg_previous && !agg_next) {
      skip <- TRUE
      message("...SKIP")
    }

    # then operate on the observation accordingly...

    if (agg_previous) {
      points_to_aggregate <- matrix(cbind(locations$lon[(row_i - 1):row_i],
                                          locations$lat[(row_i - 1):row_i]),
                                    nrow = 2, ncol = 2)
      # take the weighted mean
      new_points <- geomean(xy = points_to_aggregate,
                            w = locations$cnt_pts[(row_i - 1):row_i])
      # update focal observation with new location
      locations$lon[row_i] <- new_points[1,1]
      locations$lat[row_i] <- new_points[1,2]
      # update the count (required for future weighted averages)
      locations$cnt_pts[row_i] <- locations$cnt_pts[row_i] + locations$cnt_pts[row_i - 1] # correction: was + 1
      # update the time of the focal observation to the previous time
      locations$time[row_i] <- locations$time[row_i - 1]
      # update time_next
      locations$time_next[row_i] <- sum(locations$time_next[row_i - 1],
                                        locations$time_next[row_i])

      # drop the now-aggregated point
      locations <- slice(locations, -(row_i - 1))
      row_i <- (row_i - 1) # account for the removal of the now-aggregated point
    }

    if (agg_next) {
      points_to_aggregate <- matrix(cbind(locations$lon[row_i:(row_i + 1)],
                                          locations$lat[row_i:(row_i + 1)]),
                                    nrow = 2, ncol = 2)
      new_points <- geomean(xy = points_to_aggregate,
                            w = locations$cnt_pts[row_i:(row_i + 1)])
      # update focal observation
      locations$lon[row_i] <- new_points[1,1]
      locations$lat[row_i] <- new_points[1,2]
      # update the count (required for weighted averages)
      locations$cnt_pts[row_i] <- locations$cnt_pts[row_i] + locations$cnt_pts[row_i + 1]   # correction: was + 1
      # update time_next
      locations$time_next[row_i] <- sum(locations$time_next[row_i],
                                        locations$time_next[row_i + 1])
      # drop the now-aggregated point
      locations <- slice(locations, -(row_i + 1))
    }

    if (skip) {
      row_i <- row_i + 1
    } else {
      # calculate new dists for updated point
      locations <- locations %>%
        mutate(lon_next = dplyr::lead(lon),
               lat_next = dplyr::lead(lat),
               lon_prev = dplyr::lag(lon),
               lat_prev = dplyr::lag(lat)) %>%
        rowwise() %>%
        mutate(dist_next = distGeo(c(lon, lat), c(lon_next, lat_next)),
               dist_prev = distGeo(c(lon, lat), c(lon_prev, lat_prev))) %>%
        ungroup()
    }

    # reset
    agg_previous <- FALSE
    agg_next <- FALSE
    skip <- FALSE
  }

  locations <- locations %>%
    select(-lon_next, -lat_next, -lon_prev, -lat_prev, -dist_prev) %>%
    mutate(time_next = difftime(lead(time), time, units = "mins"))

  return(locations)
}



# gpsNearestReference = function(dTarget, dReference, MaxDistanceForMatch=0)
# {
#   dTarget$NearestReference = as.double(NA)
#
#   for(i in 1:nrow(dTarget))
#   {
#     if(!is.na(dTarget$Type[i]) && toupper(dTarget$Type[i])=='PLACE')
#     {
#       for(j in 1:nrow(dReference))
#       {
#         Distance = round(distGeo(dTarget[i,c('Long', 'Lat')], dReference[j,c('Long', 'Lat')]),1)
#         if (is.na(dTarget$NearestReference[i]) || Distance < dTarget$NearestReference[i]) dTarget$NearestReference[i] = Distance
#       }
#     }
#   }
#
#   return(dTarget)
# }


lookup_coords <- function() {

# JJC grabbed from web as model

  # Geocoding script for large list of addresses.
  # Shane Lynn 10/10/2013
  #load up the ggmap library
  # library(ggmap)
  # # get the input data
  # infile &lt;- "input"
  # data &lt;- read.csv(paste0('./', infile, '.csv'))
  # # get the address list, and append "Ireland" to the end to increase accuracy
  # # (change or remove this if your address already include a country etc.)
  # addresses = data$Address
  # addresses = paste0(addresses, ", Ireland")
  # #define a function that will process googles server responses for us.
  # getGeoDetails &lt;- function(address){
  #   #use the gecode function to query google servers
  #   geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #   #now extract the bits that we need from the returned list
  #   answer &lt;- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  #   answer$status &lt;- geo_reply$status
  #   #if we are over the query limit - want to pause for an hour
  #   while(geo_reply$status == "OVER_QUERY_LIMIT"){
  #     print("OVER QUERY LIMIT - Pausing for 1 hour at:")
  #     time &lt;- Sys.time()
  #     print(as.character(time))
  #     Sys.sleep(60*60)
  #     geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #     answer$status &lt;- geo_reply$status
  #   }
  #   #return Na's if we didn't get a match:
  #   if (geo_reply$status != "OK"){
  #     return(answer)
  #   }
  #   #else, extract what we need from the Google server reply into a dataframe:
  #   answer$lat &lt;- geo_reply$results[[1]]$geometry$location$lat
  #   answer$long &lt;- geo_reply$results[[1]]$geometry$location$lng
  #   if (length(geo_reply$results[[1]]$types) &gt; 0){
  #     answer$accuracy &lt;- geo_reply$results[[1]]$types[[1]]
  #   }
  #   answer$address_type &lt;- paste(geo_reply$results[[1]]$types, collapse=',')
  #   answer$formatted_address &lt;- geo_reply$results[[1]]$formatted_address
  #   return(answer)
  # }
  # #initialise a dataframe to hold the results
  # geocoded &lt;- data.frame()
  # # find out where to start in the address list (if the script was interrupted before):
  # startindex &lt;- 1
  # #if a temp file exists - load it up and count the rows!
  # tempfilename &lt;- paste0(infile, '_temp_geocoded.rds')
  # if (file.exists(tempfilename)){
  #   print("Found temp file - resuming from index:")
  #   geocoded &lt;- readRDS(tempfilename)
  #   startindex &lt;- nrow(geocoded)
  #   print(startindex)
  # }
  # # Start the geocoding process - address by address. geocode() function takes care of query speed limit.
  # for (ii in seq(startindex, length(addresses))){
  #   print(paste("Working on index", ii, "of", length(addresses)))
  #   #query the google geocoder - this will pause here if we are over the limit.
  #   result = getGeoDetails(addresses[ii])
  #   print(result$status)
  #   result$index &lt;- ii
  #   #append the answer to the results file.
  #   geocoded &lt;- rbind(geocoded, result)
  #   #save temporary results as we are going along
  #   saveRDS(geocoded, tempfilename)
  # }
  # #now we add the latitude and longitude to the main data
  # data$lat &lt;- geocoded$lat
  # data$long &lt;- geocoded$long
  # data$accuracy &lt;- geocoded$accuracy
  # #finally write it all to the output files
  # saveRDS(data, paste0("../data/", infile ,"_geocoded.rds"))
  # write.table(data, file=paste0("../data/", infile ,"_geocoded.csv"), sep=",", row.names=FALSE)


}


lookup_address <- function(longitude, latitude, provider = "photon", api = NULL) {

  #check inputs
  if (missing(longitude)) stop("Must provide longitude")
  if (missing(latitude)) stop("Must provide latitude")

  provider <- tolower(provider)
  if (!(provider %in% c("photon", "google"))) stop("provider must be photon or google")
  if (provider == "google" && is.null(api)) stop("Must provide api if provider == google")

  # functions
  get_response <- function(url, provider){
    response <- httr::GET(url)

    if (response$status_code != 200L) {
      stop("Error: status_code = ", response$status_code)
    } else {
      response <- httr::content(response)
    }
  }

  format_response <- function(response, provider) {

    if (provider == "photon") {

      if (length(response$features) > 1) {
        warning ("Multiple addresses found for lon = ",
                 response$features[[1]]$geometry$coordinates[[1]],
                 " and lat = ", response$features[[1]]$geometry$coordinates[[2]],
                 ". Using first address.")
      }
      housenumber <- tryCatch(response$features[[1]]$properties$housenumber,
                              error = function(e) NA_character_)
      street <- tryCatch(response$features[[1]]$properties$street,
                         error = function(e) NA_character_)
      city <- tryCatch(response$features[[1]]$properties$city,
                       error = function(e) NA_character_)
      zip <- tryCatch(response$features[[1]]$properties$postcode,
                      error = function(e) NA_character_)
      state <- tryCatch(response$features[[1]]$properties$state,
                        error = function(e) NA_character_)
      country <- tryCatch(response$features[[1]]$properties$country,
                          error = function(e) NA_character_)

      if (is.null(housenumber)) housenumber <- NA_character_
      if (is.null(street)) street <- NA_character_
      if (is.null(city)) city <- NA_character_
      if (is.null(zip)) zip <- NA_character_
      if (is.null(state)) state <- NA_character_
      if (is.null(country)) country <- NA_character_

      df <- tibble(housenumber, street, city, state, zip, country)
      df <- df %>%
        mutate(formatted_address = paste0(housenumber, " ", street, ", ", city, ", ", state, " ", zip, ', ', country),
               formatted_address = str_remove_all(formatted_address, "NA, "),
               formatted_address = str_remove_all(formatted_address, "NA "))
    }

    if (provider == "google") {

      if (length(response$results) > 1) {
        warning ("Multiple addresses found for lon = ",
                 response$results[[1]]$geometry$location$lng,
                 " and lat = ", response$results[[1]]$geometry$location$lat,
                 ". Using first address.")
      }

      housenumber <- NA_character_
      street  <- NA_character_
      city  <- NA_character_
      zip  <- NA_character_
      state  <- NA_character_
      country <- NA_character_

      for (i in seq_along(response$results[[1]]$address_components)){

        type <- response$results[[1]]$address_components[[i]]$types[[1]]
        value <- response$results[[1]]$address_components[[i]]$long_name
        if (type == "street_number") housenumber <- value
        if (type == "route") street <- value
        if (type == "locality") city <- value
        if (type == "postal_code") zip <- value
        if (type == "administrative_area_level_1") state <- value
        if (type == "country") country <- value

      }
      df <- tibble(housenumber, street, city, state, zip, country)
      df$formatted_address <- response$results[[1]]$formatted_address
      df$google_place_id <- response$results[[1]]$place_id
    }

    return(df)
  }

  # format url for provider
  if (provider == "photon") {
    url <- str_c("https://photon.komoot.io/reverse?lon=", longitude, "&lat=", latitude)
  } else {
    url <- str_c("https://maps.googleapis.com/maps/api/geocode/json?latlng=",
                 latitude, ",", longitude, "&key=", api)
  }

  df <- url %>%
    get_response(provider) %>%
    format_response(provider)

  return(df)
}

find_nearest_context <- function(lon_target, lat_target, context){
# target_lon, target_lat are coords of the place for which we are seeking a
# context match
# context is a tibble with lon and lat columns for places for which we have context
# returns a tibble with context_lon, context_lat, and context_dist, for subsequent
# can call function within ~map2_dfr() to get context for a vector of places

  get_dist <- function(lon_context, lat_context,
                       lon_target, lat_target){
    p1 <- c(lon_context, lat_context)
    p2 <- c(lon_target, lat_target)
    distGeo(p1, p2)
  }

  context <- context %>%
    dplyr::filter(variable_name == "type") %>%
    select(lon_context = lon, lat_context =  lat) %>%
    mutate(dist_context = map2_dbl(.$lon_context, .$lat_context,
                                   get_dist,
                                   lon_target = lon_target, lat_target = lat_target)) %>%
    arrange(dist_context) %>%
    slice(1)
  return(context)
}
