#' nDSM data for area of interest
#'
#' Retrieves most recent normalized digital surface model for area of interest
#' from the Vermont Center for Geographic Information, and converts it to a
#' \code{RasterBrick} object
#'
#' @param aoi sf polygon object with area of interest geometry. CRS must be
#'   defined
#'
#' @return \code{RasterBrick} object with heights above ground (m) in aoi
#' @export
#'
#' @examples
get_ndsm <- function(aoi){
  endpt <- "https://maps.vcgi.vermont.gov/arcgis/rest/services/EGC_services/IMG_VCGI_LIDARNDSM_WM_CACHE_v1/ImageServer"
  out <- arcpullr::get_image_layer(
    url = endpt,
    sf_object = sf::st_transform(aoi, sf::st_crs(3857)),
    format = 'tiff')
  return(out)
}


#' Color orthophoto for area of interest
#'
#' Retrieves most recent color orthoimagery for area of interest from the
#' Vermont Center for Geographic Information, and converts it to a
#' \code{RasterBrick} object
#'
#' @param aoi sf polygon object with area of interest geometry. CRS must be
#'   defined
#'
#' @return \code{RasterBrick} object with RGB color orthoimage for aoi
#' @export
#'
#' @examples
get_clr <- function(aoi){
  endpt <- "https://maps.vcgi.vermont.gov/arcgis/rest/services/EGC_services/IMG_VCGI_CLR_WM_CACHE/ImageServer"
  out <- arcpullr::get_image_layer(
    url = endpt,
    sf_object = sf::st_transform(aoi, sf::st_crs(3857)))
  return(out)
}


#' Landcover data for area of interest
#'
#' Retrieves 2016, 0.5m resolution Vermont landcover data for area of interest
#' from the Vermont Center for Geographic Information, and converts it to a
#' \code{RasterBrick} object
#'
#' @param aoi sf polygon object with area of interest geometry. CRS must be
#'   defined
#'
#' @return \code{RasterBrick} object with RGB color image of landcover classes
#'   in aoi
#' @export
#'
#' @examples
get_landcover <- function(aoi){
  endpt <- "https://maps.vcgi.vermont.gov/arcgis/rest/services/EGC_services/IMG_VCGI_BASELANDCOVER2016_WM_CACHE_v1/ImageServer"
  out <- arcpullr::get_image_layer(
    url = endpt,
    sf_object = sf::st_transform(aoi, sf::st_crs(3857)))
  return(out)
}


#' Wetlands in area of interest
#'
#' Retrieves 2016 Vermont wetlands data for area of interest from the Vermont
#' Center for Geographic Information, and converts it to a ??? object
#'
#' @param aoi sf polygon object with area of interest geometry. CRS must be
#'   defined
#'
#' @return \code{???} object containing wetlands in aoi
#' @export
#'
#' @examples
# get_wetlands <- function(aoi){
#   endpt <- "https://maps.vcgi.vermont.gov/arcgis/rest/services/EGC_services/VECTOR_VCGI_WETLANDS2016_WM_v1/VectorTileServer"
#   out <- arcpullr::get_image_layer(
#     url = endpt,
#     sf_object = sf::st_transform(aoi, sf::st_crs(3857)))
#   return(out)
# }
