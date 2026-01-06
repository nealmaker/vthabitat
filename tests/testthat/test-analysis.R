# Tests for analysis functions

# Helper to check if external data is available
has_vthabitat_data <- function() {
  has_data_path() &&
    dir.exists(file.path(get_data_path(), "Landlandcov_BaseLC2022"))
}

# --- land_summary tests ---

test_that("land_summary returns data frame with correct columns", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 100)
  lc <- get_landcover(test_aoi)

  result <- land_summary(lc)

  expect_s3_class(result, "data.frame")
  expect_true("cover" %in% names(result))
  expect_true("pct" %in% names(result))
})

test_that("land_summary percentages sum to 100", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 100)
  lc <- get_landcover(test_aoi)

  result <- land_summary(lc)

  expect_equal(sum(result$pct), 100, tolerance = 0.01)
})

test_that("land_summary includes expected classes", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 100)
  lc <- get_landcover(test_aoi)

  result <- land_summary(lc)

  # Should have some of the expected land cover classes
  expected_classes <- c("Tree Canopy", "Grass/Shrubs", "Water", "Buildings", "Roads")
  found <- sum(expected_classes %in% result$cover)
  expect_true(found >= 1)  # At least one expected class present
})

# --- make_canopy tests ---

test_that("make_canopy returns SpatRaster with 4 categories", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 100)
  ndsm <- get_ndsm(test_aoi)
  lc <- get_landcover(test_aoi)

  result <- make_canopy(ndsm, lc)

  expect_s4_class(result, "SpatRaster")

  # Check categories
  cats <- terra::cats(result)[[1]]
  expect_true(!is.null(cats))
  expect_equal(nrow(cats), 4)
  expect_true(all(c("shrub", "pole", "partial", "closed") %in% cats$cover))
})

test_that("make_canopy output is in correct CRS", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 100)
  ndsm <- get_ndsm(test_aoi)
  lc <- get_landcover(test_aoi)

  result <- make_canopy(ndsm, lc)

  expect_equal(terra::crs(result, describe = TRUE)$code, "32145")
})

# --- make_shrub tests ---

test_that("make_shrub validates CRS units", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 100)
  ndsm <- get_ndsm(test_aoi)
  lc <- get_landcover(test_aoi)
  canopy <- make_canopy(ndsm, lc)

  # Project to a CRS with degrees (should fail)
  canopy_latlon <- terra::project(canopy, "EPSG:4326")

  expect_error(make_shrub(canopy_latlon), "CRS must use meters")
})

test_that("make_shrub returns sf object with acres column", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 500)
  ndsm <- get_ndsm(test_aoi)
  lc <- get_landcover(test_aoi)
  canopy <- make_canopy(ndsm, lc)

  # Use small size threshold to ensure we get some results
  result <- make_shrub(canopy, size = 0.1, distance = 50)

  if (nrow(result) > 0) {
    expect_s3_class(result, "sf")
    expect_true("acres" %in% names(result))
    expect_true(all(result$acres > 0.1))
  }
})

# --- focal_fun tests (internal function) ---

test_that("focal_fun returns expected values", {
  # Test with mock data representing different canopy conditions

  # All tall trees (closed canopy)
  tall <- rep(15, 9)  # 15m heights
  expect_equal(vthabitat:::focal_fun(tall), 4)

  # Mix of tall and short (partial canopy)
  mixed <- c(15, 15, 15, 15, 2, 2, 2, 2, 2)
  expect_equal(vthabitat:::focal_fun(mixed), 3)

  # Pole timber
  poles <- rep(5, 9)  # 5m heights
  expect_equal(vthabitat:::focal_fun(poles), 2)

  # Shrub/young forest
  shrub <- rep(1.5, 9)  # 1.5m heights
  expect_equal(vthabitat:::focal_fun(shrub), 1)

  # Ground cover (should be NA)
  ground <- rep(0.3, 9)
  expect_true(is.na(vthabitat:::focal_fun(ground)))

  # NA input returns NA
  expect_true(is.na(vthabitat:::focal_fun(rep(NA, 9))))
})
