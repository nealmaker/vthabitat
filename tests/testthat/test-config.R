# Tests for configuration functions

test_that("set_data_path validates directory exists", {
  expect_error(set_data_path("/nonexistent/path/12345"),
               "Directory does not exist")
})

test_that("set_data_path accepts valid directory", {
  tmp_dir <- tempdir()
  old_path <- set_data_path(tmp_dir)
  expect_equal(get_data_path(), normalizePath(tmp_dir))
  # Restore
  if (!is.null(old_path)) options(vthabitat.data_path = old_path)
})

test_that("set_data_path returns previous value invisibly", {
  tmp_dir <- tempdir()
  # Set initial
  set_data_path(tmp_dir)
  # Set again and capture return
  old <- set_data_path(tmp_dir)
  expect_equal(old, normalizePath(tmp_dir))
})

test_that("get_data_path errors when not set", {
  # Clear the option
  old <- getOption("vthabitat.data_path")
  options(vthabitat.data_path = NULL)

  expect_error(get_data_path(), "Data path not set")

  # Restore
  options(vthabitat.data_path = old)
})

test_that("has_data_path returns correct logical", {
  old <- getOption("vthabitat.data_path")

  options(vthabitat.data_path = NULL)
  expect_false(has_data_path())

  options(vthabitat.data_path = tempdir())
  expect_true(has_data_path())

  # Restore
  options(vthabitat.data_path = old)
})
