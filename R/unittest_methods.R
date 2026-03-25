#' Check for tests/testthat and _snaps folder and count golden tests
#'
#' @param pkg_source_path Path to the root of the package source
#'
#' @return A list with:
#'   - `has_testthat`: Does tests/testthat exist?
#'   - `has_testit`: Does tests/testit exist?
#'   - `has_tests_base`: Do tests exist in tests subdirectory?
#'   - `has_snaps`: Does _snaps exist inside tests/testthat?
#'   - `n_golden_tests`: Number of snapshot files inside _snaps
#'   - `n_test_files`: Number of test-*.R files inside tests/testthat
#' @keywords internal
check_pkg_tests_and_snaps <- function(pkg_source_path) {
  message("checking package test config")
  
  test_dir <- file.path(pkg_source_path, "tests")
  testthat_path <- file.path(test_dir, "testthat")
  snaps_path <- file.path(testthat_path, "_snaps")
  
  testit_path <- file.path(test_dir, "testit")
  test_ci_path <- file.path(test_dir, "test-ci")
  test_cran_path <- file.path(test_dir, "test-cran")
  
  # Check for testthat and testit (standard and nonstandard)
  has_testthat <- dir.exists(testthat_path)
  has_testit <- dir.exists(testit_path) || (dir.exists(test_ci_path) && dir.exists(test_cran_path))
  
  # Count golden test snapshot files
  has_snaps <- dir.exists(snaps_path)
  n_golden_tests <- if (has_snaps) {
    snapshot_files <- list.files(snaps_path, recursive = TRUE, full.names = TRUE)
    length(snapshot_files)
  } else {
    0
  }
  
  # Count test-*.R files in testthat
  n_test_files <- if (has_testthat) {
    test_files <- list.files(
      testthat_path,
      pattern = "^test-.*\\.R$",
      recursive = TRUE,
      full.names = TRUE
    )
    test_files <- test_files[!grepl("_snaps", test_files)]
    length(test_files)
  } else {
    0
  }
  
  # Only check for base R test scripts if none of the known test frameworks are present
  has_tests_base <- FALSE
  if (!has_testthat && !has_testit && !dir.exists(test_ci_path) && !dir.exists(test_cran_path)) {
    base_test_files <- list.files(
      test_dir,
      pattern = "\\.R$",
      full.names = TRUE
    )
    # Exclude files in known subdirectories
    base_test_files <- base_test_files[dirname(base_test_files) == test_dir]
    has_tests_base <- length(base_test_files) > 0
  }
  
  return(list(
    has_testthat = has_testthat,
    has_snaps = has_snaps,
    has_testit = has_testit,
    has_tests_base = has_tests_base,
    n_golden_tests = n_golden_tests,
    n_test_files = n_test_files
  ))
}

#' Determine the appropriate test path for a package
#'
#' This internal utility function inspects the `test_pkg_data` structure to identify
#' which testing framework is used in the package and returns the corresponding test path.
#'
#' @param test_pkg_data A named list containing logical flags such as `has_testthat`, `has_testit`, `has_tests_base`, etc.
#' @param testdir A character string specifying the root directory where test folders are located.
#'
#' @return A character string representing the path to the test directory, or `NULL` if no known test framework is found.
#'
#' @keywords internal
get_test_path <- function(test_pkg_data, testdir) {
  if (isTRUE(test_pkg_data$has_testthat)) {
    return(file.path(testdir, "testthat"))
  } else if (isTRUE(test_pkg_data$has_testit)) {
    return(file.path(testdir, "testit"))
  } else if (
    dir.exists(file.path(testdir, "test-ci")) &&
    dir.exists(file.path(testdir, "test-cran"))
  ) {
    return(testdir)  # fallback for nonstandard testit
  } else if (isTRUE(test_pkg_data$has_tests_base)) {
    return(testdir)  # base R scripts
  }
  
  return(NULL)
}




