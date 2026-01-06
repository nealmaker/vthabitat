# Tests for AOI functions

test_that("centroid creates sf point in Vermont State Plane", {
  pt <- centroid(44.0, -72.7)

  expect_s3_class(pt, "sf")
  expect_equal(sf::st_crs(pt)$epsg, 32145)
  expect_equal(nrow(pt), 1)
  expect_true(sf::st_is(pt, "POINT"))
})

test_that("centroid accepts Vermont coordinates", {
  # Roughly center of Vermont
  pt <- centroid(44.0, -72.7)
  coords <- sf::st_coordinates(pt)

  # Should be within Vermont State Plane bounds (rough check)
  expect_true(coords[1, "X"] > 400000 && coords[1, "X"] < 600000)
  expect_true(coords[1, "Y"] > 0 && coords[1, "Y"] < 300000)
})

test_that("aoi creates sf polygon from centroid", {
  pt <- centroid(44.0, -72.7)
  area <- aoi(centroid = pt, size = 100)

  expect_s3_class(area, "sf")
  expect_equal(sf::st_crs(area)$epsg, 32145)
  expect_true(sf::st_is(area, "POLYGON"))
})
test_that("aoi errors without centroid or property", {
  expect_error(aoi(), "No property or centroid given")
})

test_that("aoi size parameter affects area", {
  pt <- centroid(44.0, -72.7)

  small_aoi <- aoi(centroid = pt, size = 100)
  large_aoi <- aoi(centroid = pt, size = 1000)

  small_area <- sf::st_area(small_aoi)
  large_area <- sf::st_area(large_aoi)

  expect_true(large_area > small_area)
})

test_that("aoi is approximately correct size", {
  pt <- centroid(44.0, -72.7)
  area <- aoi(centroid = pt, size = 100)  # 100 acres

  # Calculate area in acres
  area_m2 <- as.numeric(sf::st_area(area))
  area_acres <- area_m2 * 0.000247105

  # Should be close to 100 acres (circular buffer)
  expect_true(area_acres > 90 && area_acres < 110)
})

test_that("shp2sf reads shapefile", {
  skip_if_not(file.exists(system.file("extdata", "maker-stands.shp",
                                        package = "vthabitat")),
              "Sample shapefile not found")

  shp_path <- system.file("extdata", "maker-stands.shp", package = "vthabitat")
  result <- shp2sf(shp_path)

  expect_s3_class(result, "sf")
  expect_equal(sf::st_crs(result)$epsg, 32145)
})
