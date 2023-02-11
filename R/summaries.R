#' Landcover summary
#'
#' Summarizes percentage of land area in each of eight cover types, based on a
#' raster layer of Vermont's 2016 landcover data for a given area
#'
#' @param landcover \code{RasterBrick} object with RGB color image of VT 2016
#'   landcover classes, from \code{get_landcover}
#'
#' @return data frame of cover types and the percentage of land area covered by
#'   each
#' @export
#'
#' @examples
land_summary <- function(landcover){
  x <- summary(as.factor(arcpullr::raster_colors(landcover)$color))
  # remove NA values (outside of mask), which show up as roads
  x[which(names(x) == "#000000")] <-
    x[which(names(x) == "#000000")] - (sum(is.na(landcover@data@values))/3)
  data.frame(
    cover = landcover_key$cover[match(names(x), landcover_key$color)],
    pct = 100 * x / sum(x))
}
