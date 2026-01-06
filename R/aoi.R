#' Create area of interest (aka landscape area)
#'
#' @param property optional sf polygon object with geometry of property for
#'   which landscape context is being considered. CRS must be defined. If
#'   \code{property} is not used, \code{centroid} must be defined
#' @param centroid optional sf point object with geometry of aoi centroid. CRS
#'   must be defined. If \code{centroid} is not defined, \code{property} must be
#'   used.
#' @param size minimum size of aoi, in acres. Defaults to 2500 acres based on
#'   Audubon recommendations.
#' @param expand should aoi be allowed to expand in size, to accommodate large
#'   properties that would otherwise extend beyond aoi? TRUE or FALSE. Only used
#'   if \code{property} is defined.
#'
#' @return sf polygon object with area of interest geometry. CRS is
#'   EPSG:32145 (NAD83 / Vermont)
#' @export
#'
#' @examples
#' # Create AOI from a point location
#' pt <- centroid(44.393, -72.487)
#' landscape <- aoi(centroid = pt, size = 500)
#' print(landscape)
#'
#' \dontrun{
#' # Create AOI from a property shapefile
#' property <- shp2sf("property.shp")
#' landscape <- aoi(property = property, size = 2500, expand = TRUE)
#' }
aoi <- function(property = NA, centroid = NA, size = 2500, expand = T){
  buflength <- sqrt((size * 43560) / pi) / 3.28084 # meters

  if(any(!is.na(property))){
    # merge features for muti-feature shapefiles before getting centroid
    prop1 <- property %>% dplyr::mutate(axT5 = 1) %>% dplyr::group_by(.data$axT5) %>%
      dplyr::summarize(geometry = sf::st_union(.data$geometry)) %>% dplyr::ungroup()

    centroid <- sf::st_centroid(prop1)

    if(expand){
      coords <- sf::st_coordinates(sf::st_transform(property, sf::st_crs(4326)))
      prop_rad <-
        max(geosphere::distHaversine(
          data.frame(coords),
          data.frame(sf::st_coordinates(sf::st_transform(
            centroid, sf::st_crs(4326)))))) + 100
      buflength <- max(buflength, prop_rad)
    }
  } else if(is.na(centroid)){
    stop("No property or centroid given to define aoi.")
  }

  return(sf::st_buffer(sf::st_transform(centroid, sf::st_crs(32145)),
                       buflength))
}

check_aoi <- function(aoi) {
  # Error handling: Check if AOI is an sf object
  # Check if `aoi` is an sf polygon
  if (!inherits(aoi, "sf") ||
      !any(grepl("POLYGON", sf::st_geometry_type(aoi)))) {
    stop("Input 'aoi' must be an sf polygon object.")
  }

  # Error handling: Check if AOI has a CRS defined
  if (is.na(sf::st_crs(aoi))) {
    stop("Error: The input 'aoi' must have a defined CRS.")
  }

  # Transform AOI to a projected CRS for accurate area calculation (EPSG:3857)
  aoi_transformed <- sf::st_transform(aoi, crs = 3857)

  # Check if AOI is larger than 5000 acres (1 acre = 4046.86 square meters)
  aoi_area <- as.numeric(sf::st_area(aoi_transformed)) # Area in square meters
  if (aoi_area > 5000 * 4046.86) {
    warning("Warning: The area of interest is larger than 5000 acres, which may result in a large download.")
  }
}
