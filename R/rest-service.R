#' Fetch data from ArcREST service
#'
#' @param aoi sf polygon object with area of interest geometry. CRS must be
#'   EPSG:3857 and must be defined
#' @param base_url service url, ending in service type and layer if applicable
#'   (eg. .../ImageServer/0)
#' @param query_params named list of query parameters and their values. "format"
#'   parameter must be defined to set file extension. See service's api
#'   reference for available parameters.
#'
#' @return \code{SpatRaster} object of fetched data, w/ CRS EPSG:3857
#' @export
rest_service <- function(aoi, base_url, query_params) {
  # Check internet connection and service availability
  tryCatch({
    response <- httr::GET(base_url)
    if (httr::http_error(response)) {
      stop("Error: The ArcGIS REST service is unavailable.")
    }
  }, error = function(e) {
    stop("Error: Unable to connect to the ArcGIS REST service. Please check your internet connection.")
  })

  # # Query the service metadata to determine the highest resolution (smallest cell size)
  # metadata_url <- paste0(image_server_url, "?f=pjson")
  # metadata_response <- httr::GET(metadata_url)
  # if (httr::http_error(metadata_response)) {
  #   stop("Error: Unable to fetch service metadata to determine resolution.")
  # }
  # metadata <- jsonlite::fromJSON(httr::content(metadata_response, as = "text"))
  # highest_resolution <- min(unlist(metadata$minScale, use.names = FALSE))
  #
  # # Dynamically calculate the size parameter based on the bounding box dimensions and highest resolution
  # resolution <- highest_resolution / 2.54
  # width <- ceiling((bbox$xmax - bbox$xmin) / resolution)
  # height <- ceiling((bbox$ymax - bbox$ymin) / resolution)

  # Fetch the raster data from the Image Server
  file_ext <- paste0(".", query_params$format)

  query_url <- httr2::request(paste0(base_url, "/exportImage"))
  for (param in names(query_params)) {
    query_url <- httr2::req_url_query(query_url,
                                      !!param := query_params[[param]])
  }

  raster_response <- tryCatch({
    httr2::req_perform(
      query_url, path = tempfile(fileext = file_ext)
    )
  }, error = function(e) {
    stop("Error: Failed to fetch the raster data from the ArcGIS REST service.")
  })

  # Load the raster data into R using terra
  out <- terra::rast(raster_response$body[1])

  # Ensure the raster is in the desired CRS (EPSG:3857)
  out <- terra::project(out, "EPSG:3857")

  # Clip the raster to match the AOI's irregular shape
  out <- terra::mask(out, terra::vect(aoi))

  # Return the resulting raster
  return(out)
}
