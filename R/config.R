#' Set path to vthabitat external data
#'
#' Sets the directory path where vthabitat looks for large raster datasets
#' (land cover, wetlands, tree canopy). This should be called once at the
#' start of a session before using functions that require local data.
#'
#' @param path Character string with path to data directory. The directory
#'   should contain subdirectories or files for each dataset.
#'
#' @return Invisibly returns the previous path setting.
#' @export
#'
#' @examples
#' \dontrun{
#' set_data_path("~/vthabitat_data")
#' }
set_data_path <- function(path) {
  if (!dir.exists(path)) {
    stop("Directory does not exist: ", path)
  }
  old <- getOption("vthabitat.data_path")
  options(vthabitat.data_path = normalizePath(path))
  invisible(old)
}


#' Get path to vthabitat external data
#'
#' Returns the currently configured data directory path.
#'
#' @return Character string with path, or NULL if not set.
#' @export
#'
#' @examples
#' \dontrun{
#' # After setting data path:
#' set_data_path("~/vthabitat_data")
#' get_data_path()
#' }
get_data_path <- function() {
  path <- getOption("vthabitat.data_path")
  if (is.null(path)) {
    stop("Data path not set. Use set_data_path() to configure the location ",
         "of vthabitat raster data.")
  }
  path
}


#' Check if data path is configured
#'
#' @return Logical indicating if data path has been set.
#' @keywords internal
has_data_path <- function() {
  !is.null(getOption("vthabitat.data_path"))
}
