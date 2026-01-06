#' Create centroid from lat and lon
#'
#' Manually enter latitude and longitude to create an sf centroid object that
#' can later be used to define an area of interest, for example.
#'
#' @param lat latitude in decimal degrees
#' @param lon longitude in decimal degrees
#'
#' @return sf point object with centroid geometry. CRS is ESPG:32145 (NAD83 /
#'   Vermont)
#' @export
#'
#' @examples
#' # Create a centroid for a location in Vermont
#' pt <- centroid(44.393, -72.487)
#' print(pt)
centroid <- function(lat, lon){
  dat <- data.frame(lat = lat, lon = lon)
  cent <- sf::st_set_crs(sf::st_as_sf(dat, coords = c("lon", "lat")), 4326)
  return(sf::st_transform(cent, 32145))
}


#' Convert shapefile to sf object
#'
#' @param file path to .shp file. CRS must be ESPG:32145 (NAD83 / Vermont)
#'
#' @return sf object
#' @export
#'
#' @examples
#' \dontrun{
#' # Read a shapefile
#' property <- shp2sf("path/to/property.shp")
#' }
shp2sf <- function(file){
  sf::st_set_crs(sf::st_read(file), 32145)
}
