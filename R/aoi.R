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
#' @return returns sf polygon object with area of interest geometry. CRS is
#'   EPSG:32145 (NAD83 / Vermont)
#' @export
#'
#' @examples
aoi <- function(property = NA, centroid = NA, size = 2500, expand = T){
  geometry = axT5 = NULL # to make cmd check "no visible binding" note go away

  buflength <- sqrt((size * 43560) / pi) / 3.28084 # meters

  if(!is.na(property)){
    # merge features for muti-feature shapefiles before getting centroid
    prop1 <- property %>% dplyr::mutate(axT5 = 1) %>% dplyr::group_by(axT5) %>%
      dplyr::summarize(geometry = sf::st_union(geometry)) %>% dplyr::ungroup()

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
