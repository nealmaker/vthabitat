# Tests for analysis functions

# Helper to check if external data is available
has_vthabitat_data <- function() {
  has_data_path() &&
    dir.exists(file.path(get_data_path(), "Landlandcov_BaseLC2022"))
}

# --- land_summary tests ---

test_that("land_summary returns list with correct structure", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 100)
  lc <- get_landcover(test_aoi)
  ndsm <- get_ndsm(test_aoi)
  wet <- get_wetlands(test_aoi)
  ttype <- get_treetype(test_aoi)
  canopy <- make_canopy(ndsm, lc)
  shrub <- make_shrub(canopy, size = 0.1, distance = 50)

  result <- land_summary(lc, canopy, wet, shrub, ttype)

  expect_type(result, "list")
  expect_true("cover" %in% names(result))
  expect_true("treetype" %in% names(result))
  expect_s3_class(result$cover, "data.frame")
  expect_s3_class(result$treetype, "data.frame")
  expect_true("cover" %in% names(result$cover))
  expect_true("pct" %in% names(result$cover))
})

test_that("land_summary cover percentages sum to 100", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 100)
  lc <- get_landcover(test_aoi)
  ndsm <- get_ndsm(test_aoi)
  wet <- get_wetlands(test_aoi)
  ttype <- get_treetype(test_aoi)
  canopy <- make_canopy(ndsm, lc)
  shrub <- make_shrub(canopy, size = 0.1, distance = 50)

  result <- land_summary(lc, canopy, wet, shrub, ttype)

  expect_equal(sum(result$cover$pct), 100, tolerance = 0.5)
})

test_that("land_summary includes expected classes", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 100)
  lc <- get_landcover(test_aoi)
  ndsm <- get_ndsm(test_aoi)
  wet <- get_wetlands(test_aoi)
  ttype <- get_treetype(test_aoi)
  canopy <- make_canopy(ndsm, lc)
  shrub <- make_shrub(canopy, size = 0.1, distance = 50)

  result <- land_summary(lc, canopy, wet, shrub, ttype)

  # Should have some of the expected land cover classes
  expected_classes <- c("Developed", "Water", "Upland Closed", "Upland Partial",
                        "Upland Pole", "Forested Wetland")
  found <- sum(expected_classes %in% result$cover$cover)
  expect_true(found >= 1)  # At least one expected class present
})

# --- make_canopy tests ---

test_that("make_canopy returns SpatRaster with 5 categories", {
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
  expect_equal(nrow(cats), 5)
  expect_true(all(c("groundcover", "shrub", "pole", "partial", "closed") %in% cats$cover))
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

  # Ground cover (returns 0)
  ground <- rep(0.3, 9)
  expect_equal(vthabitat:::focal_fun(ground), 0)

  # NA input returns NA
  expect_true(is.na(vthabitat:::focal_fun(rep(NA, 9))))
})

# --- pct_to_words tests (internal function) ---

test_that("pct_to_words converts small numbers to words", {
  expect_equal(vthabitat:::pct_to_words(0), "zero")
  expect_equal(vthabitat:::pct_to_words(1), "one")
  expect_equal(vthabitat:::pct_to_words(10), "ten")
  expect_equal(vthabitat:::pct_to_words(20), "twenty")
})

test_that("pct_to_words returns numeric string for large numbers", {
  expect_equal(vthabitat:::pct_to_words(21), "21")
  expect_equal(vthabitat:::pct_to_words(50), "50")
  expect_equal(vthabitat:::pct_to_words(100), "100")
})

test_that("pct_to_words handles vectors", {
  result <- vthabitat:::pct_to_words(c(1, 5, 25))
  expect_equal(result, c("one", "five", "25"))
})

# --- describe_landscape tests ---

test_that("describe_landscape returns character string", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 100)
  lc <- get_landcover(test_aoi)
  ndsm <- get_ndsm(test_aoi)
  wet <- get_wetlands(test_aoi)
  ttype <- get_treetype(test_aoi)
  canopy <- make_canopy(ndsm, lc)
  shrub <- make_shrub(canopy, size = 0.1, distance = 50)

  summary <- land_summary(lc, canopy, wet, shrub, ttype)
  result <- describe_landscape(summary)

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  expect_true(grepl("Landscape analysis", result))
})

test_that("describe_landscape mentions forest composition", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  pt <- centroid(44.393, -72.487)
  test_aoi <- aoi(centroid = pt, size = 100)
  lc <- get_landcover(test_aoi)
  ndsm <- get_ndsm(test_aoi)
  wet <- get_wetlands(test_aoi)
  ttype <- get_treetype(test_aoi)
  canopy <- make_canopy(ndsm, lc)
  shrub <- make_shrub(canopy, size = 0.1, distance = 50)

  summary <- land_summary(lc, canopy, wet, shrub, ttype)
  result <- describe_landscape(summary)

  # Should mention hardwoods or softwoods
  expect_true(grepl("hardwood|softwood", result, ignore.case = TRUE))
})

# --- landscape_elements tests ---

test_that("landscape_elements returns list with landscape_text", {
  skip_if_not(has_vthabitat_data(), "vthabitat data not available")
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Create a small test property
  pt <- centroid(44.393, -72.487)
  property <- sf::st_buffer(pt, 100)

  result <- landscape_elements(property, yf_size = 0.1, yf_dist = 50)

  expect_type(result, "list")
  expect_true("landscape_text" %in% names(result))
  expect_type(result$landscape_text, "character")
  expect_true(nchar(result$landscape_text) > 0)
})
