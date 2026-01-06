#' nDSM data for area of interest
#'
#' Retrieves most recent normalized digital surface model for area of interest
#' from the Vermont Center for Geographic Information.
#'
#' @param aoi sf polygon object with area of interest geometry. CRS must be
#'   defined.
#'
#' @return \code{SpatRaster} object with heights above ground (m) in aoi
#' @export
#'
#' @examples
#' \dontrun{
#' # Get nDSM for an area of interest
#' pt <- centroid(44.393, -72.487)
#' my_aoi <- aoi(centroid = pt, size = 100)
#' ndsm <- get_ndsm(my_aoi)
#' terra::plot(ndsm)
#' }
get_ndsm <- function(aoi){
  # Download tiff directly - arcpullr has issues with tiff format for this service
  bbox <- sf::st_bbox(sf::st_transform(aoi, sf::st_crs(3857)))
  bbox_str <- paste(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"], sep = ",")

  url <- paste0(
    "https://maps.vcgi.vermont.gov/arcgis/rest/services/EGC_services/",
    "IMG_VCGI_LIDARNDSM_WM_CACHE_v1/ImageServer/exportImage?",
    "bbox=", bbox_str,
    "&bboxSR=3857&imageSR=3857&format=tiff&f=image"
  )

  tmp <- tempfile(fileext = ".tif")
  utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
  out <- terra::rast(tmp)

  # Transform to Vermont State Plane (EPSG:32145) for consistency with local data
  out <- terra::project(out, "EPSG:32145")

  # Crop to AOI
  aoi_transformed <- sf::st_transform(aoi, terra::crs(out))
  out <- terra::crop(out, aoi_transformed, mask = TRUE)

  return(out)
}


#' Color orthophoto for area of interest
#'
#' Retrieves most recent color orthoimagery for area of interest from the
#' Vermont Center for Geographic Information.
#'
#' @param aoi sf polygon object with area of interest geometry. CRS must be
#'   defined.
#'
#' @return \code{SpatRaster} object with RGB color orthoimage for aoi
#' @export
#'
#' @examples
#' \dontrun{
#' # Get color orthophoto for an area of interest
#' pt <- centroid(44.393, -72.487)
#' my_aoi <- aoi(centroid = pt, size = 100)
#' clr <- get_clr(my_aoi)
#' terra::plotRGB(clr)
#' }
get_clr <- function(aoi){
  endpt <- "https://maps.vcgi.vermont.gov/arcgis/rest/services/EGC_services/IMG_VCGI_CLR_WM_CACHE/ImageServer"
  out <- arcpullr::get_image_layer(
    url = endpt,
    sf_object = sf::st_transform(aoi, sf::st_crs(3857)))

  # Transform to Vermont State Plane (EPSG:32145) for consistency with local data
  out <- terra::project(out, "EPSG:32145")

  # Crop to AOI
  aoi_transformed <- sf::st_transform(aoi, terra::crs(out))
  out <- terra::crop(out, aoi_transformed, mask = TRUE)

  return(out)
}


#' Landcover data for area of interest
#'
#' Retrieves 2022 Vermont base landcover data for area of interest from local
#' raster data. Requires \code{set_data_path()} to be configured first.
#'
#' @param aoi sf polygon object with area of interest geometry. CRS must be
#'   defined.
#'
#' @return \code{SpatRaster} object with landcover class values for aoi. The
#'   raster includes category labels from the source data.
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
#' terra::plot(lc)
#'
#' # View land cover categories
#' terra::cats(lc)
#' }
get_landcover <- function(aoi) {
  data_path <- get_data_path()
  lc_dir <- file.path(data_path, "Landlandcov_BaseLC2022")
  lc_file <- file.path(lc_dir, "Landlandcov_BaseLC2022.tif")

  if (!file.exists(lc_file)) {
    stop("Land cover data not found at: ", lc_file, "\n",
         "Download from geodata.vermont.gov and place in: ", lc_dir,
         " or use set_data_path function.")
  }

  # Read raster
  lc_rast <- terra::rast(lc_file)

  # Load category labels from .vat.dbf if not already set
  if (is.null(terra::cats(lc_rast)[[1]])) {
    vat_file <- paste0(lc_file, ".vat.dbf")
    if (file.exists(vat_file)) {
      vat <- foreign::read.dbf(vat_file)
      # Assuming columns: Value, Count, and a class name column
      name_col <- setdiff(names(vat), c("Value", "Count", "OID"))[1]
      if (!is.null(name_col)) {
        levels(lc_rast) <- data.frame(value = vat$Value, label = vat[[name_col]])
      }
    }
  }

  # Crop to AOI
  aoi_transformed <- sf::st_transform(aoi, terra::crs(lc_rast))
  out <- terra::crop(lc_rast, aoi_transformed, mask = TRUE)

  return(out)
}


#' Wetlands data for area of interest
#'
#' Retrieves 2022 Vermont modeled wetlands data for area of interest from local
#' raster data. Requires \code{set_data_path()} to be configured first.
#'
#' @param aoi sf polygon object with area of interest geometry. CRS must be
#'   defined.
#'
#' @return \code{SpatRaster} object with wetland class values for aoi. Classes
#'   include Emergent, Forested, and Scrub/Shrub wetlands.
#' @export
#'
#' @examples
#' \dontrun{
#' # Set data path first
#' set_data_path("~/vthabitat_data")
#'
#' # Get wetlands for an area of interest
#' pt <- centroid(44.393, -72.487)
#' my_aoi <- aoi(centroid = pt, size = 100)
#' wetlands <- get_wetlands(my_aoi)
#' terra::plot(wetlands)
#' }
get_wetlands <- function(aoi) {
  data_path <- get_data_path()
  wet_dir <- file.path(data_path, "LandLandcov_Wetlands2022")
  wet_file <- file.path(wet_dir, "LandLandcov_Wetlands2022.tif")

  if (!file.exists(wet_file)) {
    stop("Wetlands data not found at: ", wet_file, "\n",
         "Download from geodata.vermont.gov and place in: ", wet_dir,
         " or use set_data_path function.")
  }

  # Read raster
  wet_rast <- terra::rast(wet_file)

  # Load category labels from .vat.dbf if not already set
  if (is.null(terra::cats(wet_rast)[[1]])) {
    vat_file <- paste0(wet_file, ".vat.dbf")
    if (file.exists(vat_file)) {
      vat <- foreign::read.dbf(vat_file)
      name_col <- setdiff(names(vat), c("Value", "Count", "OID"))[1]
      if (!is.null(name_col)) {
        levels(wet_rast) <- data.frame(value = vat$Value, label = vat[[name_col]])
      }
    }
  }

  # Crop to AOI
  aoi_transformed <- sf::st_transform(aoi, terra::crs(wet_rast))
  out <- terra::crop(wet_rast, aoi_transformed, mask = TRUE)

  return(out)
}


#' Hardwood and softwood canopy data for area of interest
#'
#' Retrieves 2022 Vermont modeled tree type data for area of interest from local
#' raster data. Requires \code{set_data_path()} to be configured first.
#'
#' @param aoi sf polygon object with area of interest geometry. CRS must be
#'   defined.
#'
#' @return \code{SpatRaster} object with tree type class values for aoi. Classes
#'   include Coniferous and Deciduous.
#' @export
#'
#' @examples
#' \dontrun{
#' # Set data path first
#' set_data_path("~/vthabitat_data")
#'
#' # Get canopy type for an area of interest
#' pt <- centroid(44.393, -72.487)
#' my_aoi <- aoi(centroid = pt, size = 100)
#' treetype <- treetype(my_aoi)
#' terra::plot(treetype)
#' }
get_treetype <- function(aoi) {
  data_path <- get_data_path()
  treetype_dir <- file.path(data_path, "LandLandcov_TreeCanopy2022")
  treetype_file <- file.path(treetype_dir, "LandLandcov_TreeCanopy2022.tif")

  if (!file.exists(treetype_file)) {
    stop("Tree type data not found at: ", treetype_file, "\n",
         "Download from geodata.vermont.gov and place in: ", treetype_dir,
         " or use set_data_path function.")
  }

  # Read raster
  treetype_rast <- terra::rast(treetype_file)

  # Load category labels from .vat.dbf if not already set
  if (is.null(terra::cats(treetype_rast)[[1]])) {
    vat_file <- paste0(treetype_file, ".vat.dbf")
    if (file.exists(vat_file)) {
      vat <- foreign::read.dbf(vat_file)
      name_col <- setdiff(names(vat), c("Value", "Count", "OID"))[1]
      if (!is.null(name_col)) {
        levels(treetype_rast) <- data.frame(value = vat$Value, label = vat[[name_col]])
      }
    }
  }

  # Crop to AOI
  aoi_transformed <- sf::st_transform(aoi, terra::crs(treetype_rast))
  out <- terra::crop(treetype_rast, aoi_transformed, mask = TRUE)

  return(out)
}
