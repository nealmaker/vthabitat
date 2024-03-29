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
  out <- terra::mask(out, landcover)

  levels(out) <-
    data.frame(value = 1:4, cover = c("shrub", "pole", "partial", "closed"))

  return(out)
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


#' Polygons of young forest and shrubland
#'
#' @param canopy \code{SpatRaster} object showing canopy attributes for area of
#'   interest, made with \code{make_canopy}. Currently CRS must be in meters, or
#'   areas will fail silently!
#' @param size minimum size, in acres, a shrubby area must occupy for inclusion
#' @param distance maximum distance, in feet, two shrub polygons can be from
#'   one-another and still count as a single shrubby area
#'
#' @return
#' @export
#'
#' @examples
make_shrub <- function(canopy, size = 1, distance = 100){
  canopy[canopy != 1] <- NA # 1 is shrubby
  out <- sf::st_as_sf(terra::as.polygons(canopy))
  # need single polygons to eliminate small areas, then filter out tiny remnants
  # before recombining by distance (GOOD IDEA? OR UNFAIRLY REMOVING GOOD BUT
  # BROKEN HABITAT AREAS?)
  out <- sf::st_cast(out, to = "POLYGON")
  out <- out[units::drop_units(sm2ac(sf::st_area(out))) > .1, ]
  out <- buffergroup(out, distance/3.280839895) # ft to meters
  out$acres <- units::drop_units(sm2ac(sf::st_area(out)))
  out <- out[out$acres > size, ]
  return(out)
}

sm2ac <- function(sm) return(sm*.0002471054)

buffergroup <- function(x, dist){
  # dist is buffer distance & must be in units of CRS
  y <- sf::st_cast(sf::st_union(sf::st_buffer(x, dist = dist)), to = "POLYGON")
  x$group <- unlist(sf::st_intersects(x, y))
  x <- x %>% dplyr::group_by(group) %>%
    dplyr::summarise(geometry = sf::st_union(geometry))
  return(x)
}
