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
  # out <- arcpullr::get_image_layer(
  #   url = endpt,
  #   sf_object = sf::st_transform(aoi, sf::st_crs(3857)))
  bbox <- sf::st_bbox(sf::st_transform(aoi, sf::st_crs(3857))) #some better way to clip to aoi
  url <- pasteo(endpt, "/exportImage?f=image&bbox=", bbox$xmin, ",", bbox$ymin,
                ",", bbox$xmax, ",", bbox$ymax)
  res <- httr2::req_perform(httr2::request(url))
  # need to turn response body into something I can use in R
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
  endpt <- "https://tiles.arcgis.com/tiles/BkFxaEFNwHqX3tAw/arcgis/rest/services/IMG_VCGI_BASELANDCOVER2016_WM_v1/MapServer"
  # I was using arcpullr, but it seems to have broken
  # out <- arcpullr::get_image_layer(
  #   url = endpt,
  #   sf_object = sf::st_transform(aoi, sf::st_crs(3857)))
  # The following is based on the ArcREST documentation
  # & is returning a 404 error: operation not supported.
  # Is it because this is really a raster tile service thing?
  # (Says map service)
  bbox <- sf::st_bbox(sf::st_transform(aoi, sf::st_crs(3857))) #some better way to clip to aoi
  url <- paste0(endpt, "/export&bbox=", bbox$xmin, ",", bbox$ymin,
                ",", bbox$xmax, ",", bbox$ymax, "&f=html")
  # This seems to be the tile service version, which is probably right.
  # I think row, column, width, height are based on tiles, so have to
  # calculate them like how codellama was trying to (below)
  # not sure if it will allow export, as 'export tiles allowed' is FALSE
  # url <- paste0(endpt, "/tilemap/", <level>, "/", <row>, "/", <column>, "/",
  # <width>, "/", <height>)

  res <- httr2::req_perform(httr2::request(url))
  # somehow turn response body into R-friendly raster
  return(out)
}

# Here is a url for vector tile services based on ArcREST documentation,
# seems to work, but need token to access data:
# 'https://tiles.arcgis.com/tiles/BkFxaEFNwHqX3tAw/arcgis/rest/services/VECTOR_VCGI_WETLANDS2016_WM_v1/VectorTileServer/exportTiles?&exportExtent={"spatialReference":{"wkid":102100,"latestWkid":3857},"xmax":1.936330167439007E7,"xmin":1.8878385166948937E7,"ymax":-5133377.883934237,"ymin":-5466031.831031308}&levels=0'
# export extent is bounding box; can also use polygon to clip to shape

# Based on codellama suggestion. This is wrong, but has some maybe useful things
# I might refer to later:
# fetch_vector_tile_data <- function(aoi, tile_service_url, layer_name) {
#   # Determine the tile extent and zoom level based on the AOI
#   tile_extent <- sf::st_bbox(sf::st_transform(aoi, 3857)) # Web Mercator (EPSG:3857)
#   tile_zoom <- 19 # use most zoomed in zoom level available
#
#   # Define tile coordinates: tiles numbered from upper left, size based on the zoom level
#   # CALCULATIONS UNTESTED FROM CODELLAMA
#   x = floor((tile_extent$xmin + 20037508.34) / (256 * 2 ^ tile_zoom))
#   y = floor((20037508.34 - tile_extent$ymax) / (256 * 2 ^ tile_zoom))
#   xmax <- ceiling((tile_extent$xmax + 20037508.34) / (256 * 2 ^ tile_zoom))
#   ymax <- ceiling((20037508.34 - tile_extent$ymin) / (256 * 2 ^ tile_zoom))
#
#   # Construct the tile request URL
#   # THIS CONNECTS, BUT CONTENT OF REPLY IS EMPTY
#   # COULD BE THAT I HAVEN"T TRIED VALID TILE COORDINATAES (x, y, xmax, ymax)
#   # DON"T KNOW IF 'layer' & 'bbox' DO ANYTHING
#   # OR EVEN IF I CAN FETCH MULTIPLE TILES AT ONCE
#   # !!!see: https://developers.arcgis.com/rest/services-reference/enterprise/export-tiles-vector-tile-service/
#   tile_url <- paste0(tile_service_url, "/tile/", tile_zoom, "/",
#                      x, "/", y, ".pbf?layer=", layer_name, "&bbox=", x, ",", y,
#                      ",", xmax, ",", ymax)
#
#   # Send a GET request to the vector tile service
#   req <- httr2::request(tile_url)
#   res <- req |> httr2::req_perform(req)
#
#   # Check if the request was successful
#   if (status_code(response) != 200) {
#     stop("Error fetching vector tile data:", status_code(response))
#   }
#
#   # LOOK INTO osmextract PACKAGE, WHICH MIGHT PARSE .pbf FILES
#   # FROM PHIND: One key function in the osmextract package is oe_vectortranslate(), which converts .pbf files into .gpkg (GeoPackage) format. This conversion is performed using ogr2ogr through the vectortranslate utility in sf::gdal_utils(). The .gpkg format is chosen because it offers database capabilities such as random access and querying, which are crucial for handling large datasets like those found in OSM 1.
# }

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
#   # arcpullr never worked here:
#   out <- arcpullr::get_image_layer(
#     url = endpt,
#     sf_object = sf::st_transform(aoi, sf::st_crs(3857)))
#   return(out)
# }
