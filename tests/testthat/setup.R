# Test setup - runs before all tests

# Try to set data path if the data directory exists
# This allows tests to run when data is available locally

# Get package root directory
pkg_root <- testthat::test_path("../..")

data_candidates <- c(
  file.path(pkg_root, "../vthabitat_data"),  # Sibling to package
  Sys.getenv("VTHABITAT_DATA"),               # Environment variable
  "~/vthabitat_data",                          # Home directory
  "../vthabitat_data",                         # Relative to working dir
  "../../vthabitat_data"
)

for (path in data_candidates) {
  if (nzchar(path) && dir.exists(path)) {
    options(vthabitat.data_path = normalizePath(path))
    message("vthabitat data found at: ", normalizePath(path))
    break
  }
}
