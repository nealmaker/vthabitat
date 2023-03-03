#' Landscape context map
#'
#' @param clr rgb raster of color orthophoto for area of interest
#' @param landcover rgb raster of landover for area of interest
#' @param prop optional polygon layer of property
#'
#' @return Returns map of area of interest, showing its landcover overlain on an
#'   aerial photo and, optionally, the property boundaries.
#' @export
#'
#' @examples
map_context <- function(clr, landcover, prop = NA){
  if(is.na(prop)){
    tmap::tm_shape(clr) + tmap::tm_rgb(alpha = .9) +
      tmap::tm_shape(landcover) + tmap::tm_rgb(alpha = .35)
  } else {
    tmap::tm_shape(clr) + tmap::tm_rgb(alpha = .9) +
      tmap::tm_shape(landcover) + tmap::tm_rgb(alpha = .35) +
      tmap::tm_shape(prop) + tmap::tm_borders(lwd = 4, col = "#470602")
  }
}


#' Tree canopy map
#'
#' @param canopy categorical raster of tree canopy
#' @param prop optional polygon layer of property
#'
#' @return Returns map of area of interest, showing its tree canopy types and,
#'   optionally, the property boundaries.
#' @export
#'
#' @examples
map_canopy <- function(canopy, prop = NA){
  if(is.na(prop)){
    tmap::tm_shape(canopy) +
      tmap::tm_raster(title = "tree canopy",
                      palette = c("#9e4103", "#efde9b", "#85b57c", "#0a3f00"),
                      alpha = .8)
  } else {
    tmap::tm_shape(canopy) +
      tmap::tm_raster(title = "tree canopy",
                      palette = c("#9e4103", "#efde9b", "#85b57c", "#0a3f00"),
                      alpha = .8) +
      tmap::tm_shape(prop) + tmap::tm_borders(lwd = 4, col = "#470602")
  }
}


#' Shrub/young forest map
#'
#' @param clr rgb raster of color orthophoto for area of interest
#' @param landcover rgb raster of landover for area of interest
#' @param shrub polygon layer of shrub/young forest areas
#' @param prop optional polygon layer of property
#'
#' @return Returns map of area of interest, showing shrub/young forest areas
#'   overlain on an aerial photo and, optionally, the property boundaries.
#' @export
#'
#' @examples
map_shrub <- function(clr, landcover, shrub, prop = NA){
  if(is.na(prop)){
    tmap::tm_shape(clr) + tmap::tm_rgb(alpha = .8) +
      tmap::tm_shape(landcover) + tmap::tm_rgb(alpha = .25) +
      tmap::tm_shape(shrub) +
      tmap::tm_polygons(col = "#9e4103", border.col = "black", lwd = .2)
  } else {
    tmap::tm_shape(clr) + tmap::tm_rgb(alpha = .8) +
      tmap::tm_shape(landcover) + tmap::tm_rgb(alpha = .25) +
      tmap::tm_shape(shrub) +
      tmap::tm_polygons(col = "#9e4103", border.col = "black", lwd = .2) +
      tmap::tm_shape(prop) + tmap::tm_borders(lwd = 4, col = "#470602")
  }
}
