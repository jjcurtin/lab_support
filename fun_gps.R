# Functions to support working with location data

# Required packages
library(leaflet)


plot_places <- function(places) {
# places is a tibble that has two required and one optional column:
# lat (numeric), lon (numeric), and info(character).
# Other columns are ignored.

  if (!("info" %in% names(places))){
    places$info <-as.character(1:nrow(places))
  }

  map <-  leaflet() %>%
    addTiles() %>%
    addCircleMarkers(data = places,
                     lng = ~lon, lat = ~lat,
                     radius = 1.5, color = "red", opacity = 1,
                     popup = ~info) %>%
    addMeasure(position = "bottomleft",
               primaryLengthUnit = "meters",
               primaryAreaUnit = "sqmeters")

  return(map)
}


plot_tracks <- function(locs, gap = 5) {

  locs <- locs %>%
    arrange(time) %>%
    mutate(time_next = difftime(lead(time), time, units = "mins"))

  map <-  leaflet() %>%
    addTiles(group = 'Color') %>% # Add default OpenStreetMap map tiles
    addProviderTiles(providers$OpenStreetMap.BlackAndWhite, group = 'B&W') #B&W OSM tiles


  i <- 1
  while(i < nrow(locs)) { # need at least two points left for a new track
    track <- tibble(lat = double(), lon = double())  # make new empty track

    track <- track %>%
      add_row(lat = locs$lat[i], lon = locs$lon[i])

    while(i < nrow(locs) &&
          !is.na(locs$time_next[i]) &&
          locs$time_next[i] <= gap) {
      i <- i + 1
      track <- track %>%
        add_row(lat = locs$lat[i], lon = locs$lon[i])
    }

    #display this track if it has at least 2 points
    if(nrow(track) > 1){
      map <- map %>%
        addPolylines(data = track,
                        lng = ~lon, lat = ~lat,
                        color = "red", weight = 2,
                        opacity = 1, group = 'tracks')
    }

    i <- i + 1  #advance to next trackpoint (first in next track)
  }
  map <- map %>%
    addLayersControl(baseGroups = c('color', 'B&W'),
                     overlayGroups = c('places', 'tracks'),
                     options = layersControlOptions(collapsed = FALSE)) %>%
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
# Takes a tibble of places, groups them sequentially into
# into groups that differ by no more than max_dist meters
# from the previous points in the group.  It then returns
# a weight mean of the the location for each group

  if (nrow(places) > 1){
    places <- mutate(places, place_grp = NA)
    max_grp <- 0

    for(i in 1:(nrow(places) - 1)){

      if (is.na(places$place_grp[i])) {
        max_grp <- max_grp + 1
        current_grp <- max_grp
        places$place_grp[i] <- current_grp
      } else {
        current_grp <- places$place_grp[i]
      }

      for (j in (i + 1):nrow(places)){
        if(distGeo(c(places$lon[i], places$lat[i]),
                   c(places$lon[j], places$lat[j])) <= max_dist){
          places$place_grp[j] <- current_grp
        }
      }
    }

    agg_places <- tibble(most_recent_date = Date(), lat = double(), lon = double(), cnt_pts = double())
    for (i in 1:max_grp){
      places_grouped <- places %>%
        filter(place_grp == i)

      if (nrow(places_grouped) > 1) {
        xy <- places_grouped %>%
          select(lon, lat)
        w <- places_grouped$cnt_pts
        xy_new <- geomean(xy, w)
        agg_places <- agg_places %>%
          add_row(most_recent_date = max(places_grouped$date),
                              lat = xy_new[1,2],
                              lon = xy_new[1,1],
                              cnt_pts = sum(places_grouped$cnt_pts))
      }else {
        agg_places <- agg_places %>%
          add_row(select(places_grouped, most_recent_date = date, lat, lon, cnt_pts))
      }
    }
  } else  agg_places <- select(places, most_recent_date = date, lat, lon, cnt_pts)

  return(agg_places)
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
           lon_next = lead(lon),
           lat_next = lead(lat),
           lon_prev = lag(lon),
           lat_prev = lag(lat)) %>%
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

    message(str_c("...checking row ", row_i))

    # evaluate the row as:
    # point to be aggregated with previous row
    if (row_i != 1 && # not first row # row_i != nrow(locations) && # not last row
        locations$dist_prev[row_i] < locations$dist_next[row_i] &&
        locations$dist_prev[row_i] <= max_dist) {
      agg_previous <- TRUE
      message("aggregating row with previous")
    }

    # point to be aggregated with next row
    if (# (!row_i >= nrow(locations)) &&
      row_i != 1 &&
      locations$dist_next[row_i] <= locations$dist_prev[row_i] &&
      locations$dist_next[row_i] <= max_dist) {
      agg_next <- TRUE
      message("aggregating row with next")
    }

    # neither
    if (!agg_previous && !agg_next) {
      skip <- TRUE
      message("not aggregating row, leaving as is")
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
      message(".... observation aggregated")
    }

    if (agg_next) {
      message("aggregating with the next observation")
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
      message("...observation aggregating")
    }

    if (skip) {
      message("...leaving observation")
      row_i <- row_i + 1
    } else {
      # calculate new dists for updated point
      locations <- locations %>%
        mutate(lon_next = lead(lon),
               lat_next = lead(lat),
               lon_prev = lag(lon),
               lat_prev = lag(lat)) %>%
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
    select(-lon_next, -lat_next, -lon_prev, -lat_prev, dist_next, -dist_prev) %>%
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



