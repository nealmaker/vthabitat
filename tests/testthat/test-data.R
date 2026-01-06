# Tests for data retrieval functions

# Helper to check if external data is available
has_vthabitat_data <- function() {
  has_data_path() &&
    dir.exists(file.path(get_data_path(), "Landlandcov_BaseLC2022"))
}

# --- Network-dependent tests (API calls) ---

test_that("get_ndsm returns SpatRaster in correct CRS", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 50)

  result <- get_ndsm(test_aoi)

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::crs(result, describe = TRUE)$code, "32145")
  expect_true(terra::hasValues(result))
})

test_that("get_clr returns SpatRaster with 3 layers", {
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 50)

  result <- get_clr(test_aoi)

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::nlyr(result), 3)  # RGB
  expect_equal(terra::crs(result, describe = TRUE)$code, "32145")
})

# --- Local data tests ---

test_that("get_landcover returns SpatRaster with categories", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 50)

  result <- get_landcover(test_aoi)

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::crs(result, describe = TRUE)$code, "32145")

  # Should have category labels
  cats <- terra::cats(result)[[1]]
  expect_true(!is.null(cats))
  expect_true(nrow(cats) > 0)
})

test_that("get_landcover includes expected land cover classes", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 100)

  result <- get_landcover(test_aoi)
  cats <- terra::cats(result)[[1]]

  # Check for expected column
  expect_true("Class" %in% names(cats) || "label" %in% names(cats))
})

test_that("get_landcover errors with helpful message when data missing", {
  skip_if(has_vthabitat_data(), "Test requires missing data")

  old_path <- getOption("vthabitat.data_path")
  set_data_path(tempdir())  # Empty temp dir

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 50)

  expect_error(get_landcover(test_aoi), "Land cover data not found")

  # Restore
  if (!is.null(old_path)) options(vthabitat.data_path = old_path)
})

test_that("get_wetlands returns SpatRaster with categories", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")
  skip_if_not(
    dir.exists(file.path(get_data_path(), "LandLandcov_Wetlands2022")),
    "Wetlands data not available"
  )

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 100)

  result <- get_wetlands(test_aoi)

  expect_s4_class(result, "SpatRaster")
  expect_equal(terra::crs(result, describe = TRUE)$code, "32145")
})
