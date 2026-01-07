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
#' \dontrun{
#' # Set data path first
#' set_data_path("~/vthabitat_data")
#'
#' # Get data for area of interest
#' pt <- centroid(44.393, -72.487)
#' my_aoi <- aoi(centroid = pt, size = 100)
#' clr <- get_clr(my_aoi)
#' lc <- get_landcover(my_aoi)
#'
#' # Create landscape context map
#' map_context(clr, lc)
#' }
map_context <- function(clr, landcover, prop = NA){
  if (all(is.na(prop))) {
    tmap::tm_shape(clr) + tmap::tm_rgb(alpha = .9) +
      tmap::tm_shape(landcover) + tmap::tm_raster(alpha = .35)
  } else {
    # merge features for muti-feature property shapefiles
    # prop <- prop %>% dplyr::mutate(axT5 = 1) %>% dplyr::group_by(axT5) %>%
    #   dplyr::summarize(geometry = sf::st_union(geometry)) %>% dplyr::ungroup()

    tmap::tm_shape(clr) + tmap::tm_rgb(alpha = .9) +
      tmap::tm_shape(landcover) + tmap::tm_raster(alpha = .35) +
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
#' \dontrun{
#' # Set data path first
#' set_data_path("~/vthabitat_data")
#'
#' # Get data and create canopy raster
#' pt <- centroid(44.393, -72.487)
#' my_aoi <- aoi(centroid = pt, size = 100)
#' ndsm <- get_ndsm(my_aoi)
#' lc <- get_landcover(my_aoi)
#' canopy <- make_canopy(ndsm, lc)
#'
#' # Create canopy map
#' map_canopy(canopy)
#' }
map_canopy <- function(canopy, prop = NA){
  canopy[canopy == 0] <- NA
  levels(canopy) <- data.frame(value = 1:4,
                               cover = c("shrub", "pole", "partial", "closed"))
  if (all(is.na(prop))) {
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
#' \dontrun{
#' # Set data path first
#' set_data_path("~/vthabitat_data")
#'
#' # Get data and create shrub polygons
#' pt <- centroid(44.393, -72.487)
#' my_aoi <- aoi(centroid = pt, size = 500)
#' clr <- get_clr(my_aoi)
#' ndsm <- get_ndsm(my_aoi)
#' lc <- get_landcover(my_aoi)
#' canopy <- make_canopy(ndsm, lc)
#' shrub <- make_shrub(canopy)
#'
#' # Create shrub/young forest map
#' map_shrub(clr, lc, shrub)
#' }
map_shrub <- function(clr, landcover, shrub, prop = NA){
  if (all(is.na(prop))) {
    tmap::tm_shape(clr) + tmap::tm_rgb(alpha = .8) +
      tmap::tm_shape(landcover) + tmap::tm_raster(alpha = .25) +
      tmap::tm_shape(shrub) +
      tmap::tm_polygons(col = "#9e4103", border.col = "black", lwd = .2)
  } else {
    tmap::tm_shape(clr) + tmap::tm_rgb(alpha = .8) +
      tmap::tm_shape(landcover) + tmap::tm_raster(alpha = .25) +
      tmap::tm_shape(shrub) +
      tmap::tm_polygons(col = "#9e4103", border.col = "black", lwd = .2) +
      tmap::tm_shape(prop) + tmap::tm_borders(lwd = 4, col = "#470602")
  }
}
