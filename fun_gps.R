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
