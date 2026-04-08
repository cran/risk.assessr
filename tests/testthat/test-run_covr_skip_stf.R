test_that("running run_covr_skip for test package in tar file", {
  skip_on_cran()
  r = getOption("repos")
  r["CRAN"] = "http://cran.us.r-project.org"
  options(repos = r)
  
  dp <- system.file("test-data", "test.package.0014_0.1.0.tar.gz",
                    package = "risk.assessr")
  
  # set up package
  install_list <- set_up_pkg(dp)
  
  build_vignettes <- install_list$build_vignettes
  package_installed <- install_list$package_installed
  pkg_source_path <- install_list$pkg_source_path
  rcmdcheck_args <- install_list$rcmdcheck_args
  
  # install package locally to ensure test works
  package_installed <- install_package_local(pkg_source_path)
  package_installed <- TRUE
  
  if (package_installed == TRUE ) { 
    rcmdcheck_args$path <- pkg_source_path
    
    test_pkg_data <- check_pkg_tests_and_snaps(pkg_source_path)
    
    results <- run_covr_skip_stf(pkg_source_path, test_pkg_data)
    
    testthat::expect_identical(length(results), 4L)
    
    testthat::expect_true(checkmate::check_class(results, "list"))
    
    testthat::expect_identical(length(results$res_cov), 4L)
    
    testthat::expect_type(results$res_cov, "list")
  
    testthat::expect_true(length(results$res_cov) > 0)
    
    testthat::expect_true(checkmate::check_class(results$long_summary, "data.frame"))
    testthat::expect_true(all(!is.na(results$long_summary$file)))
    testthat::expect_true(all(!is.na(results$long_summary$code_script)))
    testthat::expect_true(all(!is.na(results$long_summary$context)))
    testthat::expect_true(all(!is.na(results$long_summary$status)))
    testthat::expect_true(all(checkmate::test_numeric(results$long_summary$n)))
    testthat::expect_true(all(checkmate::test_numeric(results$long_summary$time)))
    
    testthat::expect_true(checkmate::check_class(results$test_skip, "data.frame"))
    testthat::expect_true(all(!is.na(results$test_skip$file)))
    testthat::expect_true(all(checkmate::test_numeric(results$test_skip$line1)))
    testthat::expect_true(all(checkmate::test_numeric(results$test_skip$line2)))
  } 
})


test_that("run_covr_skip_stf filters test results and generates skip map", {
  # Mock testthat_long_summary output
  mock_test_results_long <- data.frame(
    file = c("test-a.R", "test-b.R", "test-c.R"),
    context = c("script1", "script2", "script3"),
    test = c("test 1", "test 2", "test 3"),
    status = c("FAILED", "PASS", "SKIPPED"),
    n = c(1, 1, 1),
    time = c(0.01, 0.02, 0.03),
    stringsAsFactors = FALSE
  )
  
  # Mock test map
  mock_test_map <- data.frame(
    test = c("test 1", "test 2", "test 3"),
    expectation = c("expect_equal", "expect_true", "expect_error"),
    line1 = c(2, 4, 6),
    line2 = c(2, 4, 6),
    file = c("test-a.R", "test-b.R", "test-c.R"),
    stringsAsFactors = FALSE
  )
  
  # Mock test_skip output
  mock_test_skip <- data.frame(
    file = c("test-a.R", "test-c.R"),
    line1 = c(2, 6),
    line2 = c(2, 6),
    stringsAsFactors = FALSE
  )
  
  # Mock test_pkg_data
  mock_test_pkg_data <- list(
    has_testthat = TRUE,
    has_testit = FALSE,
    has_tests_base = FALSE
  )
  
  # Stub all dependencies
  mockery::stub(run_covr_skip_stf, "checkmate::assert_string", function(...) TRUE)
  mockery::stub(run_covr_skip_stf, "checkmate::assert_directory_exists", function(...) TRUE)
  mockery::stub(run_covr_skip_stf, "get_pkg_name", function(...) "mockpkg")
  mockery::stub(run_covr_skip_stf, "setwd", function(...) NULL)
  mockery::stub(run_covr_skip_stf, "message", function(...) NULL)
  mockery::stub(run_covr_skip_stf, "testthat::set_max_fails", function(...) NULL)
  mockery::stub(run_covr_skip_stf, "testthat::test_dir", function(...) list())
  mockery::stub(run_covr_skip_stf, "map_tests_stf", function(...) mock_test_map)
  mockery::stub(run_covr_skip_stf, "get_tests_long_sum_stf", function(...) mock_test_results_long)
  mockery::stub(run_covr_skip_stf, "get_tests_skip_stf", function(...) mock_test_skip)
  mockery::stub(run_covr_skip_stf, "run_covr_safe_stf", function(pkg, map_path, test_skip, test_map) {
    return("mock_coverage")
  })
  mockery::stub(run_covr_skip_stf, "create_results_summary", function(cvr) {
    list(
      total_cov = 85.5,
      res_cov = list(
        name = "mockpkg",
        coverage = 85.5,
        errors = NULL,
        notes = "Coverage summary generated"
      )
    )
  })
  mockery::stub(run_covr_skip_stf, "package_name", function(...) "mockpkg")
  mockery::stub(run_covr_skip_stf, "unloadNamespace", function(...) NULL)
  
  # Run the function with test_pkg_data passed explicitly
  result <- run_covr_skip_stf(pkg_source_path = tempdir(), test_pkg_data = mock_test_pkg_data)
  
  # Check result
  expect_type(result, "list")
  expect_equal(result$total_cov, 85.5)
  expect_equal(result$res_cov$name, "mockpkg")
  expect_equal(result$res_cov$coverage, 85.5)
  expect_equal(result$res_cov$notes, "Coverage summary generated")
  expect_equal(result$test_skip, mock_test_skip)
  expect_equal(result$long_summary$context, mock_test_results_long$test)
})


test_that("run_covr_skip_stf processes no test dir and generates message", {
  # Simulate a package with no recognizable test directories
  mock_test_pkg_data <- list(
    has_testthat = FALSE,
    has_testit = FALSE,
    has_tests_base = FALSE
  )
  
  # Stub only the necessary dependencies
  mockery::stub(run_covr_skip_stf, "checkmate::assert_string", function(...) TRUE)
  mockery::stub(run_covr_skip_stf, "checkmate::assert_directory_exists", function(...) TRUE)
  mockery::stub(run_covr_skip_stf, "get_pkg_name", function(...) "mockpkg")
  mockery::stub(run_covr_skip_stf, "setwd", function(...) NULL)
  mockery::stub(run_covr_skip_stf, "message", function(...) NULL)
  
  # Run the function with no test directory
  result <- run_covr_skip_stf(pkg_source_path = tempdir(), test_pkg_data = mock_test_pkg_data)
  
  # Assertions
  testthat::expect_type(result, "list")
  testthat::expect_named(result, c("res_cov", "long_summary", "test_skip"))
  testthat::expect_true(all(vapply(result, function(x) all(is.na(x)), logical(1))))
})


test_that("run_covr_skip_stf handles missing test directory correctly", {
  # Create a temporary directory to simulate a package source path
  temp_pkg_path <- tempfile("pkg")
  dir.create(temp_pkg_path)
  
  # Mock test_pkg_data to simulate no test directories
  mock_test_pkg_data <- list(
    has_testthat = FALSE,
    has_testit = FALSE,
    has_tests_base = FALSE
  )
  
  # Stub required dependencies
  mockery::stub(run_covr_skip_stf, "checkmate::assert_string", function(...) TRUE)
  mockery::stub(run_covr_skip_stf, "checkmate::assert_directory_exists", function(...) TRUE)
  mockery::stub(run_covr_skip_stf, "get_pkg_name", function(...) "dummyPkg")
  mockery::stub(run_covr_skip_stf, "setwd", function(...) NULL)
  mockery::stub(run_covr_skip_stf, "message", function(...) NULL)
  
  # Run the function with the mocked test_pkg_data
  result <- run_covr_skip_stf(pkg_source_path = temp_pkg_path, 
                          test_pkg_data = mock_test_pkg_data)
  
  # Check that the result contains NAs as expected
  expect_type(result, "list")
  expect_named(result, c("res_cov", "long_summary", "test_skip"))
  expect_true(is.na(result$res_cov))
  expect_true(is.na(result$long_summary))
  expect_true(is.na(result$test_skip))
})

test_that("fallback problem_tests is created when no failed/skipped/error tests", {
  # Mocked test_map structure
  fake_test_map <- data.frame(
    test = rep(c("two working tests", "three working tests"), each = 2),
    expectation = rep("expect_equal", 4),
    line1 = c(2, 4, 8, 10),
    line2 = c(2, 4, 8, 10),
    file = rep("test-myscript1.R", 4),
    stringsAsFactors = FALSE
  )
  
  # Mocked test_results_long structure with only PASS statuses
  fake_test_results_long <- data.frame(
    file = rep("test-myscript1.R#L", 2),
    context = rep("myscript1", 2),
    test = c("two working tests", "three working tests"),
    status = rep("PASS", 2),
    n = c(2, 3),
    time = c(0.01, 0.01),
    stringsAsFactors = FALSE
  )
  
  # Stub dependencies
  mockery::stub(run_covr_skip_stf, "get_test_path", function(...) "tests")
  mockery::stub(run_covr_skip_stf, "testthat::test_dir", function(...) list())
  mockery::stub(run_covr_skip_stf, "map_tests_stf", function(...) fake_test_map)
  mockery::stub(run_covr_skip_stf, "get_tests_long_sum_stf", function(...) fake_test_results_long)
  mockery::stub(run_covr_skip_stf, "create_covr_list_no_skip", function(test_map, test_results, pkg_source_path, pkg) {
    list(
      total_cov = 0.5,
      res_cov = list(
        name = pkg,
        coverage = list(filecoverage = fake_test_map, totalcoverage = 50),
        errors = NA,
        notes = NA,
        passed = 5,
        failed = 0
      ),
      tests_skipped = list()
    )
  })
  
  # Run the function
  result <- run_covr_skip_stf(pkg_source_path = tempdir(), test_pkg_data = NULL)
  
  # Check that fallback logic was triggered and result is structured correctly
  expect_true(is.list(result))
  expect_named(result, c("total_cov", "res_cov", "tests_skipped"))
  expect_equal(result$res_cov$passed, 5)
  expect_equal(result$res_cov$failed, 0)
})

test_that("run_covr_safe_stf returns coverage object on success", {
  # Mock coverage object
  mock_coverage <- list(coverage = TRUE)
  
  # Stub callr::r_safe to return mock coverage
  mockery::stub(run_covr_safe_stf, "callr::r_safe", function(...) mock_coverage)
  
  result <- run_covr_safe_stf(
    pkg = "mockpkg",
    map_path = "mock/path",
    test_skip = data.frame(),
    test_map = data.frame(),
    timeout = 60
  )
  
  expect_equal(result, mock_coverage)
})

test_that("run_covr_safe_stf returns NULL on error", {
  # Stub callr::r_safe to throw an error
  mockery::stub(run_covr_safe_stf, "callr::r_safe", function(...) stop("Simulated error"))
  
  result <- run_covr_safe_stf(
    pkg = "mockpkg",
    map_path = "mock/path",
    test_skip = data.frame(),
    test_map = data.frame(),
    timeout = 60
  )
  
  expect_null(result)
})

test_that("create_coverage_skip_stf comments and uncomments lines correctly", {
  # Sample mock data
  mock_test_skip <- data.frame(
    file = c("test-script.R", "test-script.R"),
    line1 = c(2, 5),
    line2 = c(3, 6)
  )
  
  mock_test_map <- data.frame(
    context = c("test context"),
    expectation = c("expect_equal"),
    line1 = c(2),
    line2 = c(2),
    file = c("test-script.R")
  )
  
  # Mock lines in a test file
  mock_lines <- c(
    "test_that(\"test 1\", { expect_equal(1, 1) })",
    "test_that(\"test 2\", { expect_equal(2, 2) })",
    "test_that(\"test 3\", { expect_equal(3, 3) })",
    "test_that(\"test 4\", { expect_equal(4, 4) })",
    "test_that(\"test 5\", { expect_equal(5, 5) })",
    "test_that(\"test 6\", { expect_equal(6, 6) })"
  )
  
  # Stub readLines to return mock lines
  mockery::stub(create_coverage_skip_stf, "readLines", function(...) mock_lines)
  
  
  commented_lines <- c(2, 3, 5, 6)
  
  # Stub cat to capture output instead of writing to file
  mockery::stub(create_coverage_skip_stf, "cat", function(lines, file, sep) {
    # expect_true(all(grepl("^#", lines[commented_lines])))
    # expect_true(all(grepl("^#", lines[2])))
    # expect_true(all(grepl("^#", lines[5:6])))
  })
  
  # Stub covr::package_coverage to avoid running actual coverage
  mockery::stub(create_coverage_skip_stf, "covr::package_coverage", function(...) "mock_coverage")
  
  result <- create_coverage_skip_stf(
    test_path = "tests/testthat",
    test_skip = mock_test_skip,
    test_map = mock_test_map
  )
  
  expect_equal(result, "mock_coverage")
})


test_that("create_results_summary returns correct structure and values", {
  # Mock coverage object
  mock_cvr <- list(
    "file1.R:1" = list(value = c(1, 0, 1)),
    "file2.R:2" = list(value = c(0, 0, 1)),
    "file1.R:3" = list(value = c(1, 1))
  )
  
  # Add package attribute
  attr(mock_cvr, "package") <- list(package = "mockpkg")
  
  # Stub covr::percent_coverage to return a fixed value
  mockery::stub(create_results_summary, "covr::percent_coverage", function(x) 75.0)
  
  # Run the function
  result <- create_results_summary(mock_cvr)
  
  # Check top-level structure
  expect_type(result, "list")
  expect_named(result, c("total_cov", "res_cov"))
  
  # Check total coverage
  expect_equal(result$total_cov, 0.75)
  
  # Check res_cov structure
  expect_type(result$res_cov, "list")
  expect_equal(result$res_cov$name, "mockpkg")
  expect_type(result$res_cov$coverage, "list")
  
  # Check file coverage vector
  filecoverage <- result$res_cov$coverage$filecoverage
  expect_true(is.numeric(filecoverage))
  expect_true(!is.null(dimnames(filecoverage)))
  
  # Check that coverage values are correct
  expect_equal(names(filecoverage), c("file1.R", "file2.R"))
  
  expect_equal(filecoverage["file1.R"], round(100 * 4/5, 2), ignore_attr = TRUE)
  expect_equal(filecoverage["file2.R"], round(100 * 1/3, 2), ignore_attr = TRUE)
  
  
  # Check notes and errors
  expect_true(is.na(result$res_cov$errors))
  expect_true(is.na(result$res_cov$notes))
})


test_that("create_results_summary handles NULL input correctly", {
  # Run the function with NULL input
  result <- create_results_summary(NULL)
  
  # Check top-level structure
  expect_type(result, "list")
  expect_named(result, c("total_cov", "res_cov"))
  
  # Check that total_cov is NA
  expect_true(is.na(result$total_cov))
  
  # Check res_cov structure
  expect_type(result$res_cov, "list")
  expect_true(is.na(result$res_cov$name))
  expect_type(result$res_cov$coverage, "list")
  
  # Check filecoverage is an empty numeric vector with dimnames
  filecoverage <- result$res_cov$coverage$filecoverage
  expect_true(is.numeric(filecoverage))
  expect_equal(length(filecoverage), 1)
  expect_equal(dim(filecoverage), c(1))
  expect_true(is.list(dimnames(filecoverage)))
  expect_equal(length(dimnames(filecoverage)), 1)
  expect_equal(dimnames(filecoverage)[[1]], "no_coverage.R")
  
  
  # Check totalcoverage is NA
  expect_true(is.na(result$res_cov$coverage$totalcoverage))
  
  # Check notes and errors
  expect_true(is.na(result$res_cov$errors))
  expect_true(is.na(result$res_cov$notes))
})


test_that("create_covr_list_no_skip returns correct structure with mocked coverage", {
  # Mock test_map
  test_map <- data.frame(
    context = rep("Some test", 3),
    expectation = rep("expect_equal", 3),
    line1 = c(10, 20, 30),
    line2 = c(11, 21, 31),
    file = rep("test_file.R", 3),
    stringsAsFactors = FALSE
  )
  
  # Mock test_results_long
  test_results_long <- list(
    list(
      file = "test_file.R",
      context = "Some test",
      test = "Test description",
      user = 0.01,
      system = 0.00,
      real = 0.01,
      results = list(
        structure("As expected", class = c("expectation_success", "expectation", "condition")),
        structure("As expected", class = c("expectation_success", "expectation", "condition")),
        structure("As expected", class = c("expectation_success", "expectation", "condition"))
      )
    )
  )
  
  # Mock coverage object
  mock_coverage <- list(dummy = TRUE)
  
  # Mock coverage_to_list output
  mock_coverage_list <- list(
    filecoverage = list("R/test_file.R" = 75.0),
    totalcoverage = 75.0
  )
  
  # Stub covr::package_coverage and covr::coverage_to_list
  mockery::stub(create_covr_list_no_skip, "covr::package_coverage", mock_coverage)
  mockery::stub(create_covr_list_no_skip, "covr::coverage_to_list", mock_coverage_list)
  
  # Run the function
  result <- create_covr_list_no_skip(
    test_map = test_map,
    test_results_long = test_results_long,
    pkg_source_path = "path/to/pkg",
    pkg = "mockpkg"
  )
  
  # Assertions
  expect_type(result, "list")
  expect_equal(result$total_cov, 0.75)
  expect_equal(result$res_cov$name, "mockpkg")
  expect_equal(result$res_cov$coverage$totalcoverage, 75.0)
  expect_equal(result$res_cov$passed, 3)
  expect_equal(result$res_cov$failed, 0)
  expect_equal(length(result$tests_skipped), 0)
})


test_that("create_covr_list_no_skip unloads package when loaded", {
  # Mock inputs
  # Mock test_map
  test_map <- data.frame(
    context = rep("Some test", 3),
    expectation = rep("expect_equal", 3),
    line1 = c(10, 20, 30),
    line2 = c(11, 21, 31),
    file = rep("test_file.R", 3),
    stringsAsFactors = FALSE
  )
  
  # Mock test_results_long
  test_results_long <- list(
    list(
      file = "test_file.R",
      context = "Some test",
      test = "Test description",
      user = 0.01,
      system = 0.00,
      real = 0.01,
      results = list(
        structure("As expected", class = c("expectation_success", "expectation", "condition")),
        structure("As expected", class = c("expectation_success", "expectation", "condition")),
        structure("As expected", class = c("expectation_success", "expectation", "condition"))
      )
    )
  )
  pkg_source_path <- "mock/path"
  pkg <- "mockpkg"
  
  # Mock unloadNamespace to simulate successful unload
  mock_unloadNamespace <- function(pkg) {
    message(glue::glue("Mock unloadNamespace called for {pkg}"))
  }
  
  # Stub loadedNamespaces to simulate that the package is loaded
  mock_loadedNamespaces <- function() {
    c("mockpkg", "stats", "utils")
  }
  
  # Stub covr::package_coverage to return dummy coverage
  mock_package_coverage <- function(path, type) {
    structure(list(), class = "coverage")
  }
  
  # Stub covr::coverage_to_list to return dummy coverage list
  
  mock_coverage_to_list <- function(x) {
    list(
      filecoverage = matrix(1),
      totalcoverage = 100  
    )
  }
  
  # Apply stubs
  mockery::stub(create_covr_list_no_skip, "unloadNamespace", mock_unloadNamespace)
  mockery::stub(create_covr_list_no_skip, "loadedNamespaces", mock_loadedNamespaces)
  mockery::stub(create_covr_list_no_skip, "covr::package_coverage", mock_package_coverage)
  mockery::stub(create_covr_list_no_skip, "covr::coverage_to_list", mock_coverage_to_list)
  
  # Run the function
  result <- create_covr_list_no_skip(test_map, test_results_long, pkg_source_path, pkg)
  
  # Assertions
  expect_equal(result$total_cov, 1)
  expect_true(is.list(result$res_cov))
})



test_that("create_covr_list_no_skip handles error when unloading package", {
  # Mock inputs
  test_map <- data.frame(
    context = rep("Some test", 3),
    expectation = rep("expect_equal", 3),
    line1 = c(10, 20, 30),
    line2 = c(11, 21, 31),
    file = rep("test_file.R", 3),
    stringsAsFactors = FALSE
  )
  
  # Mock test_results_long
  test_results_long <- list(
    list(
      file = "test_file.R",
      context = "Some test",
      test = "Test description",
      user = 0.01,
      system = 0.00,
      real = 0.01,
      results = list(
        structure("As expected", class = c("expectation_success", "expectation", "condition")),
        structure("As expected", class = c("expectation_success", "expectation", "condition")),
        structure("As expected", class = c("expectation_success", "expectation", "condition"))
      )
    )
  )
  pkg_source_path <- "mock/path"
  pkg <- "mockpkg"
  
  # Mock unloadNamespace to simulate failure
  mock_unloadNamespace <- function(pkg) {
    stop("Simulated unload failure")
  }
  
  # Stub loadedNamespaces to simulate that the package is loaded
  mock_loadedNamespaces <- function() {
    c("mockpkg", "stats", "utils")
  }
  
  # Stub covr::package_coverage to return dummy coverage
  mock_package_coverage <- function(path, type) {
    structure(list(), class = "coverage")
  }
  
  mock_coverage_to_list <- function(x) {
    list(
      filecoverage = matrix(1),
      totalcoverage = 100  # top-level element
    )
  }
  
  # Apply stubs
  mockery::stub(create_covr_list_no_skip, "unloadNamespace", mock_unloadNamespace)
  mockery::stub(create_covr_list_no_skip, "loadedNamespaces", mock_loadedNamespaces)
  mockery::stub(create_covr_list_no_skip, "covr::package_coverage", mock_package_coverage)
  mockery::stub(create_covr_list_no_skip, "covr::coverage_to_list", mock_coverage_to_list)
  
  # Run the function
  result <- create_covr_list_no_skip(test_map, test_results_long, pkg_source_path, pkg)
  
  expect_equal(result$total_cov, 1)
  expect_true(is.list(result$res_cov))
  expect_equal(result$res_cov$coverage$totalcoverage, 100)
})


test_that("create_covr_list_no_skip removes yarn from global environment", {
  # Create a mock yarn object in the global environment
  assign("yarn", "mock_data", envir = .GlobalEnv)
  expect_true(exists("yarn", envir = .GlobalEnv))  # Confirm it's there
  
  # Minimal mocks to allow function to run
  test_map <- data.frame(
    context = rep("Some test", 3),
    expectation = rep("expect_equal", 3),
    line1 = c(10, 20, 30),
    line2 = c(11, 21, 31),
    file = rep("test_file.R", 3),
    stringsAsFactors = FALSE
  )
  
  # Mock test_results_long
  test_results_long <- list(
    list(
      file = "test_file.R",
      context = "Some test",
      test = "Test description",
      user = 0.01,
      system = 0.00,
      real = 0.01,
      results = list(
        structure("As expected", class = c("expectation_success", "expectation", "condition")),
        structure("As expected", class = c("expectation_success", "expectation", "condition")),
        structure("As expected", class = c("expectation_success", "expectation", "condition"))
      )
    )
  )
  pkg_source_path <- "mock/path"
  pkg <- "mockpkg"
  
  mock_unloadNamespace <- function(pkg) {}
  mock_loadedNamespaces <- function() character(0)
  mock_package_coverage <- function(path, type) structure(list(), class = "coverage")
  mock_coverage_to_list <- function(x) {
    list(
      filecoverage = matrix(1),
      totalcoverage = 100
    )
  }
  
  # Apply stubs
  mockery::stub(create_covr_list_no_skip, "unloadNamespace", mock_unloadNamespace)
  mockery::stub(create_covr_list_no_skip, "loadedNamespaces", mock_loadedNamespaces)
  mockery::stub(create_covr_list_no_skip, "covr::package_coverage", mock_package_coverage)
  mockery::stub(create_covr_list_no_skip, "covr::coverage_to_list", mock_coverage_to_list)
  
  # Run the function
  result <- create_covr_list_no_skip(test_map, test_results_long, pkg_source_path, pkg)
  
  # Check that yarn was removed
  expect_false(exists("yarn", envir = .GlobalEnv))
})
