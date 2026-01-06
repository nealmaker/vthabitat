#' Landcover summary
#'
#' Summarizes percentage of land area in each cover type, based on a raster
#' layer of Vermont's 2022 base landcover data for a given area.
#'
#' @param landcover \code{SpatRaster} object with landcover classes from
#'   \code{get_landcover}. Must have category labels set.
#'
#' @return data frame with columns \code{cover} (land cover class name) and
#'   \code{pct} (percentage of non-NA area in each class)
#' @export
#'
#' @examples
#' \dontrun{
#' # Set data path first
#' set_data_path("~/vthabitat_data")
#'
#' # Get landcover for an area of interest
#' pt <- centroid(44.393, -72.487)
#' my_aoi <- aoi(centroid = pt, size = 100)
#' lc <- get_landcover(my_aoi)
#'
#' # Summarize land cover percentages
#' land_summary(lc)
#' }
land_summary <- function(landcover) {
  # Get frequency table of cell values
  freq_table <- terra::freq(landcover)

  # Remove NA row if present
  freq_table <- freq_table[!is.na(freq_table$value), ]

  # terra::freq() returns category labels directly in the value column
  # when the raster has categories set
  total_cells <- sum(freq_table$count)
  data.frame(
    cover = freq_table$value,
    pct = 100 * freq_table$count / total_cells
  )
}
