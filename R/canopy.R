#' Raster of forest canopy type
#'
#' Create SpatRaster showing forest canopy attributes in area of interest, based
#' on nDSM and landcover data
#'
#' @param ndsm \code{SpatRaster} or \code{RasterBrick} object with heights above
#'   ground (m) for area of interest, from \code{get_ndsm}
#' @param landcover \code{SpatRaster} object with landcover classes for area of
#'   interest, from \code{get_landcover}. Must have category labels set.
#'
#' @return \code{SpatRaster} object showing canopy attributes for area of
#'   interest, including: (1) shrub and young forest cover, (2) poletimber, (3)
#'   partial overstories, and (4) closed overstories
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
#' ndsm <- get_ndsm(my_aoi)
#' lc <- get_landcover(my_aoi)
#'
#' # Create canopy classification
#' canopy <- make_canopy(ndsm, lc)
#' terra::plot(canopy)
#' }
make_canopy <- function(ndsm, landcover) {
  # Convert to SpatRaster if needed
  if (!inherits(ndsm, "SpatRaster")) {
    ndsm <- terra::rast(ndsm)
  }
  if (!inherits(landcover, "SpatRaster")) {
    landcover <- terra::rast(landcover)
  }

  # Classify nDSM into canopy types using focal analysis
  out <- terra::focal(ndsm,
                      w = matrix(rep(1, 9), nrow = 3),
                      fun = focal_fun)

  # Create vegetation mask from landcover
  # Keep only tree canopy and grass/shrub classes, set others to NA
  cats <- terra::cats(landcover)[[1]]
  if (!is.null(cats) && nrow(cats) > 0) {
    # Find the value and label columns (handle different naming conventions)
    val_col <- intersect(names(cats), c("value", "Value"))[1]
    label_col <- setdiff(names(cats), c("value", "Value", "ID", "count", "Count"))[1]

    if (!is.na(val_col) && !is.na(label_col)) {
      # Find vegetated classes by label (case-insensitive match)
      veg_pattern <- "tree|canopy|grass|shrub|vegetation"
      veg_values <- cats[[val_col]][grepl(veg_pattern, cats[[label_col]], ignore.case = TRUE)]
      if (length(veg_values) == 0) {
        warning("No vegetation classes found in landcover. Using all non-NA as mask.")
        veg_mask <- !is.na(landcover)
      } else {
        # Create reclassification matrix: veg classes -> 1, others -> 0
        all_values <- cats[[val_col]]
        rcl <- cbind(all_values, ifelse(all_values %in% veg_values, 1, 0))
        veg_mask <- terra::classify(landcover, rcl)
      }
    } else {
      warning("Could not identify category columns. Using all non-NA as mask.")
      veg_mask <- !is.na(landcover)
    }
  } else {
    warning("Landcover has no category labels. Using all non-NA as mask.")
    veg_mask <- !is.na(landcover)
  }

  # Resample mask to match nDSM resolution if needed
  if (!terra::compareGeom(out, veg_mask, stopOnError = FALSE)) {
    veg_mask <- terra::resample(veg_mask, out, method = "near")
  }

  # Mask canopy classification to vegetated areas only
  out <- terra::mask(out, veg_mask, maskvalues = 0)

  # Set category labels
  levels(out) <-
    data.frame(value = 0:4,
               cover = c("groundcover", "shrub", "pole", "partial", "closed"))

  return(out)
}


focal_fun <- function(x, ...){
  if(is.na(x[ceiling(length(x))])) y <- NA # keeps mask
  else if(base::mean(x > 9.144, na.rm = T) > .7) y <- 4 # closed overstory
  else if(base::mean(x > 9.144, na.rm = T) > .3) y <- 3 # partial overstory
  else if(base::mean(x > 3.048, na.rm = T) > .5) y <- 2 # pole stand
  else if(base::mean(x > 0.6096 & x <= 3.048, na.rm = T) > .25) y <- 1 # shrubby
  else y <- 0 # ground cover

  ifelse(y >= 0, y, NA)
}


#' Polygons of young forest and shrubland
#'
#' @param canopy \code{SpatRaster} object showing canopy attributes for area of
#'   interest, made with \code{make_canopy}. CRS must use meters as linear unit.
#' @param size minimum size, in acres, a shrubby area must occupy for inclusion
#' @param distance maximum distance, in feet, two shrub polygons can be from
#'   one-another and still count as a single shrubby area
#'
#' @return \code{sf} object containing polygons of shrub/young forest areas
#'   that meet the minimum size threshold, with an \code{acres} column.
#' @export
#'
#' @examples
#' \dontrun{
#' # Set data path first
#' set_data_path("~/vthabitat_data")
#'
#' # Get data and create canopy raster
#' pt <- centroid(44.393, -72.487)
#' my_aoi <- aoi(centroid = pt, size = 500)
#' ndsm <- get_ndsm(my_aoi)
#' lc <- get_landcover(my_aoi)
#' canopy <- make_canopy(ndsm, lc)
#'
#' # Extract shrub/young forest polygons (min 1 acre, within 100 ft)
#' shrub <- make_shrub(canopy, size = 1, distance = 100)
#' plot(shrub)
#' }
make_shrub <- function(canopy, size = 1, distance = 100){
  # Validate CRS uses meters
  crs_units <- sf::st_crs(canopy)$units
  if (is.null(crs_units) || !crs_units %in% c("m", "metre", "meter")) {
    stop("CRS must use meters as linear unit. ",
         "Current CRS units: ", ifelse(is.null(crs_units), "undefined", crs_units),
         "\nConsider reprojecting with terra::project() to a metric CRS ",
         "(e.g., EPSG:32618 for Vermont).")
  }

  canopy[canopy != 1] <- NA # 1 is shrubby
  out <- sf::st_as_sf(terra::as.polygons(canopy))
  # need single polygons to eliminate small areas, then filter out tiny remnants
  # before recombining by distance (GOOD IDEA? OR UNFAIRLY REMOVING GOOD BUT
  # BROKEN HABITAT AREAS?)
  out <- suppressWarnings(sf::st_cast(out, to = "POLYGON"))
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
  x <- x %>% dplyr::group_by(.data$group) %>%
    dplyr::summarise(geometry = sf::st_union(.data$geometry))
  return(x)
}
