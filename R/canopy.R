#' Raster of forest canopy type
#'
#' Create SpatRaster showing forest canopy attributes in area of interest, based
#' on nDSM and landcover data
#'
#' @param ndsm \code{RasterBrick} object with heights above ground (m) for area
#'   of interest, from \code{get_ndsm}
#' @param landcover \code{RasterBrick} object with RGB color image of VT 2016
#'   landcover classes for area of interest, from \code{get_landcover}
#'
#' @return \code{SpatRaster} object showing canopy attributes for area of
#'   interest, including: (1) shrub and young forest cover, (2) poletimber, (3)
#'   partial overstories, and (4) closed overstories
#' @export
#'
#' @examples
make_canopy <- function(ndsm, landcover){
  ndsm <- terra::rast(ndsm)
  landcover <- terra::rast(landcover)
  # This should require certain cell size to ensure reproducability, but doesn't
  # yet! Related to resolution?
  out <- terra::focal(ndsm,
                      w = matrix(rep(1, 9), nrow = 3),
                      fun = focal_fun)
  # all un-vegetated land to NA in landcover
  terra::RGB(landcover) <- 1:3
  landcover <- terra::colorize(landcover, to = "col")
  colrs <- grDevices::rgb(terra::coltab(landcover)[[1]][,2:4],
                          maxColorValue = 255)
  togo <- terra::coltab(landcover)[[1]]$values[
    which(colrs %in% landcover_key$color[c(1,2,4,5,6,8)])]
  landcover <- terra::app(landcover, fun = function(i){
    ifelse(i %in% togo, NA, i)
  })
  # mask by vegetated so buildings etc aren't included
  return(terra::mask(out, landcover))
}


focal_fun <- function(x, ...){
  if(is.na(x[ceiling(length(x))])) y <- NA # keeps mask
  else if(base::mean(x > 9.144, na.rm = T) > .7) y <- 4 # closed overstory
  else if(base::mean(x > 9.144, na.rm = T) > .3) y <- 3 # partial overstory
  else if(base::mean(x > 3.048, na.rm = T) > .5) y <- 2 # pole stand
  else if(base::mean(x > 0.6096 & x <= 3.048, na.rm = T) > .25) y <- 1 # shrubby
  else y <- NA # ground cover not shown

  ifelse(y > 0, y, NA)
}

