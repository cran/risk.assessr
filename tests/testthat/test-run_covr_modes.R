test_that("loads datasets using data() if no files in data directory", {
  dummy_env <- new.env()
  
  mockery::stub(load_package_datasets, "requireNamespace", function(pkg, quietly) TRUE)
  mockery::stub(load_package_datasets, "find.package", function(pkg) "/mock/path")
  mockery::stub(load_package_datasets, "dir.exists", function(path) FALSE)
  
  # Stub `data()` to handle both usages
  mockery::stub(load_package_datasets, "data", function(..., list = NULL, package = NULL, envir = NULL) {
    if (is.null(list)) {
      # Simulate data(package = pkg_name)
      return(list(results = matrix(c("dataset1", "dataset2"), ncol = 1, dimnames = list(NULL, "Item"))))
    } else {
      # Simulate data(list = dataset, package = pkg_name, envir = env)
      assign(list, paste0("loaded_", list), envir = envir)
    }
  })
  
  result <- load_package_datasets("fakepkg", env = dummy_env)
  
  expect_true("dataset1" %in% result)
  expect_true("dataset2" %in% result)
  expect_equal(dummy_env$dataset1, "loaded_dataset1")
  expect_equal(dummy_env$dataset2, "loaded_dataset2")
})



test_that("run_covr_modes handles testit framework correctly", {
  # Mocked test_pkg_data with has_testit = TRUE
  mock_check_pkg_tests_and_snaps <- function(pkg_source_path) {
    list(has_testthat = FALSE, has_testit = TRUE, has_tests_base = FALSE)
  }
  
  # Mocked covr_list with NA coverage
  mock_run_coverage <- function(pkg_source_path, covr_timeout) {
    list(
      total_cov = NA,
      res_cov = list(
        coverage = list(filecoverage = c(NA, NA))
      )
    )
  }
  
  # Stub for run_covr_skip_nstf
  mock_run_covr_skip_nstf <- function(pkg_source_path, test_pkg_data) {
    list(
      total_cov = 42,
      res_cov = list(
        coverage = list(filecoverage = matrix(1)),
        notes = "NSTF coverage run"
      )
    )
  }
  
  
  # Mocked get_pkg_desc to return a package name
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  mock_unloadNamespace <- function(pkg) {
    message(glue::glue("Mock unloadNamespace called for {pkg}"))
  }
  
  # Stub all dependencies
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", mock_check_pkg_tests_and_snaps)
  mockery::stub(run_covr_modes, "run_coverage", mock_run_coverage)
  mockery::stub(run_covr_modes, "run_covr_skip_nstf", mock_run_covr_skip_nstf)
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  
  
  
  # Run the function
  result <- run_covr_modes("dummy/path")
  
  # Check that the NSTF path was taken
  expect_equal(result$total_cov, 42)
  expect_equal(result$res_cov$notes, "NSTF coverage run")
})


test_that("loads various file types from data directory", {
  dummy_env <- new.env()
  
  mockery::stub(load_package_datasets, "requireNamespace", function(pkg, quietly) TRUE)
  mockery::stub(load_package_datasets, "find.package", function(pkg) "/mock/path")
  mockery::stub(load_package_datasets, "dir.exists", function(path) TRUE)
  
  # Simulate multiple file types
  mockery::stub(load_package_datasets, "list.files", function(path, full.names) {
    c(
      "/mock/path/data/data1.rda",
      "/mock/path/data/data2.csv",
      "/mock/path/data/data3.tsv",
      "/mock/path/data/data4.txt",
      "/mock/path/data/data5.json",
      "/mock/path/data/data6.R"
    )
  })
  
  # Simulate file extensions
  mockery::stub(load_package_datasets, "tools::file_ext", function(file) {
    sub(".*\\.(.*)$", "\\1", file)
  })
  
  # Simulate file names without extensions
  mockery::stub(load_package_datasets, "tools::file_path_sans_ext", function(file) {
    sub("\\.[^.]*$", "", basename(file))
  })
  
  # Stub each file type loader
  mockery::stub(load_package_datasets, "load", function(file, envir) assign("data1", "rda_loaded", envir = envir))
  mockery::stub(load_package_datasets, "read.csv", function(file, header) data.frame(a = 1))
  mockery::stub(load_package_datasets, "read.table", function(file, header, sep = "") data.frame(b = 2))
  mockery::stub(load_package_datasets, "requireNamespace", function(pkg, quietly) TRUE)
  mockery::stub(load_package_datasets, "jsonlite::fromJSON", function(file) list(c = 3))
  mockery::stub(load_package_datasets, "sys.source", function(file, envir) assign("data6", "R_loaded", envir = envir))
  
  result <- load_package_datasets("mockpkg", env = dummy_env)
  
  expect_true(all(c("data1", "data2", "data3", "data4", "data5", "data6") %in% result))
  expect_equal(dummy_env$data1, "rda_loaded")
  expect_equal(dummy_env$data2$a, 1)
  expect_equal(dummy_env$data3$b, 2)
  expect_equal(dummy_env$data4$b, 2)
  expect_equal(dummy_env$data5$c, 3)
  expect_equal(dummy_env$data6, "R_loaded")
})


test_that("load_package_datasets returns NULL when package is not installed", {
  # Mock requireNamespace to simulate package not installed
  mockery::stub(load_package_datasets, "requireNamespace", function(pkg_name, quietly) FALSE)
  
  # Run the function
  result <- load_package_datasets("fakepkg")
  
  # Check that result is NULL
  expect_null(result)
})


test_that("cleanup_and_return_null finds project root and removes files", {
  # Create a dummy environment
  dummy_env <- new.env()
  dummy_env$x <- 1
  dummy_env$y <- 2
  
  # Stub getwd to return a mock path
  mockery::stub(cleanup_and_return_null, "getwd", function() "/mock/project")
  mockery::stub(cleanup_and_return_null, "normalizePath", function(path) path)
  # Stub file.exists to simulate project root and file existence
  mockery::stub(cleanup_and_return_null, "file.exists", function(path) {
    if (grepl("\\.Rproj$|DESCRIPTION$|\\.git$", path)) return(TRUE)
    if (grepl("polr\\.pdf$|silhouette-ex\\.ps$", path)) return(TRUE)
    return(FALSE)
  })
  
  # Stub file.remove to simulate file deletion
  mockery::stub(cleanup_and_return_null, "file.remove", function(path) TRUE)
  
  # Stub ls to simulate variables in the environment
  mockery::stub(cleanup_and_return_null, "ls", function(envir) c("x", "y"))
  mockery::stub(cleanup_and_return_null, "normalizePath", function(path) path)
  # Stub rm to simulate environment cleanup
  mockery::stub(cleanup_and_return_null, "rm", function(list, envir) {
    for (var in list) rm(list = var, envir = envir)
  })
  
  result <- cleanup_and_return_null(env = dummy_env)
  
  expect_null(result)
  expect_false(exists("x", envir = dummy_env))
  expect_false(exists("y", envir = dummy_env))
})

test_that("cleanup_and_return_null returns NULL when project root not found", {
  dummy_env <- new.env()
  dummy_env$x <- 1
  
  mockery::stub(cleanup_and_return_null, "getwd", function() "/mock/project")
  mockery::stub(cleanup_and_return_null, "file.exists", function(path) FALSE)
  mockery::stub(cleanup_and_return_null, "normalizePath", function(path) path)
  mockery::stub(cleanup_and_return_null, "ls", function(envir) "x")
  mockery::stub(cleanup_and_return_null, "rm", function(list, envir) {
    for (var in list) rm(list = var, envir = envir)
  })
  
  result <- cleanup_and_return_null(env = dummy_env)
  
  expect_null(result)
  expect_false(exists("x", envir = dummy_env))
})


test_that("returns functions with no tests", {
  mock_df <- data.frame(
    source_file = c("file1.R", "file2.R"),
    test_file = c(NA, "test_file2.R"),
    stringsAsFactors = FALSE
  )
  
  result <- get_function_no_tests(mock_df)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$source_file, "file1.R")
  expect_equal(result$comment, "No tests found")
})

test_that("returns comment when all functions have tests", {
  mock_df <- data.frame(
    source_file = c("file1.R", "file2.R"),
    test_file = c("test_file1.R", "test_file2.R"),
    stringsAsFactors = FALSE
  )
  
  result <- get_function_no_tests(mock_df)
  
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$source_file))
  expect_equal(result$comment, "all functions have at least 1 test")
})

test_that("handles empty input gracefully", {
  mock_df <- data.frame(
    source_file = character(0),
    test_file = character(0),
    stringsAsFactors = FALSE
  )
  
  result <- get_function_no_tests(mock_df)
  
  expect_equal(nrow(result), 1)
  expect_true(is.na(result$source_file))
  expect_equal(result$comment, "all functions have at least 1 test")
})



test_that("run_covr_modes handles standard testing framework with valid coverage", {
  pkg_source_path <- "mock/path"
  
  # Stub check_pkg_tests_and_snaps to simulate standard testthat presence
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(has_testthat = TRUE, has_tests_base = FALSE, has_testit = FALSE)
  })
  
  # Mocked get_pkg_desc to return a package name
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  mock_unloadNamespace <- function(pkg) {
    message(glue::glue("Mock unloadNamespace called for {pkg}"))
  }
  
  # Stub run_coverage to return complete mock coverage data
  mockery::stub(run_covr_modes, "run_coverage", function(...) {
    list(
      total_cov = 85,
      res_cov = list(
        name = "mockpkg",
        coverage = list(
          filecoverage = matrix(1, nrow = 1, dimnames = list("some_function")),
          totalcoverage = 85
        )
      )
    )
  })
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  
  result <- run_covr_modes(pkg_source_path)
  
  expect_true(is.list(result))
  expect_equal(result$total_cov, 85)
})


test_that("run_covr_modes triggers fallback to run_covr_skip_stf when coverage is NA", {
  pkg_source_path <- "mock/path"
  
  # Mocked get_pkg_desc to return a package name
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  mock_unloadNamespace <- function(pkg) {
    message(glue::glue("Mock unloadNamespace called for {pkg}"))
  }
  
  # Stub check_pkg_tests_and_snaps to simulate standard testthat presence
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(has_testthat = TRUE, has_tests_base = FALSE, has_testit = FALSE)
  })
  
  # Stub run_coverage to return NA values to trigger fallback
  mockery::stub(run_covr_modes, "run_coverage", function(...) {
    list(
      total_cov = NA,
      res_cov = list(
        name = "mockpkg",
        coverage = list(
          filecoverage = NA,
          totalcoverage = NA
        )
      )
    )
  })
  
  # Stub run_covr_skip_stf to return fallback coverage data
  mockery::stub(run_covr_modes, "run_covr_skip_stf", function(...) {
    list(
      total_cov = 42,
      res_cov = list(
        name = "mockpkg",
        coverage = list(
          filecoverage = matrix(1, nrow = 1, dimnames = list("fallback_function")),
          totalcoverage = 42
        )
      )
    )
  })
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  
  result <- run_covr_modes(pkg_source_path)
  
  expect_true(is.list(result))
  expect_equal(result$total_cov, 42)
  expect_equal(rownames(result$res_cov$coverage$filecoverage), "fallback_function")
})



test_that("run_covr_modes handles non-standard testing framework", {
  pkg_source_path <- "mock/path"
  
  # Mocked get_pkg_desc to return a package name
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  # Mock unloadNamespace to avoid side effects
  mock_unloadNamespace <- function(pkg) {
    message(glue::glue("Mock unloadNamespace called for {pkg}"))
  }
  
  # Stub check_pkg_tests_and_snaps to simulate non-standard test presence
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(has_testthat = FALSE, has_tests_base = TRUE, has_testit = FALSE)
  })
  
  # Stub run_covr_skip_nstf to return mock coverage data
  mockery::stub(run_covr_modes, "run_covr_skip_nstf", function(...) {
    list(total_cov = 50, res_cov = list(name = "mockpkg", coverage = list(totalcoverage = 50)))
  })
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  
  result <- run_covr_modes(pkg_source_path)
  
  expect_true(is.list(result))
  expect_equal(result$total_cov, 50)
})


test_that("run_covr_modes handles no testing configuration", {
  pkg_source_path <- "mock/path"
  
  # Mocked get_pkg_desc to return a package name
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  # Mock unloadNamespace to avoid side effects
  mock_unloadNamespace <- function(pkg) {
    message(glue::glue("Mock unloadNamespace called for {pkg}"))
  }
  
  # Stub all dependencies correctly
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(has_testthat = FALSE, has_tests_base = FALSE, has_testit = FALSE)
  })
  
  # Run the function
  result <- run_covr_modes(pkg_source_path)
  
  # Assertions
  expect_true(is.list(result))
  expect_equal(result$total_cov, 0)
  expect_match(result$res_cov$errors, "No recognised standard or non-standard testing configuration")
})


test_that("run_covr_modes unloads package when loaded", {
  pkg_source_path <- "mock/path"
  
  # Mock get_pkg_desc to return a known package
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  # Mock unloadNamespace to simulate successful unload
  mock_unloadNamespace <- function(pkg) {
    message(glue::glue("Mock unloadNamespace called for {pkg}"))
  }
  
  # Stub loadedNamespaces to simulate that the package is loaded
  mock_loadedNamespaces <- function() {
    c("mockpkg", "stats", "utils")
  }
  
  # Stub dependencies
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  mockery::stub(run_covr_modes, "loadedNamespaces", mock_loadedNamespaces)
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(has_testthat = FALSE, has_tests_base = FALSE, has_testit = FALSE)
  })
  
  result <- run_covr_modes(pkg_source_path)
  
  expect_equal(result$total_cov, 0)
  expect_match(result$res_cov$errors, "No recognised standard or non-standard testing configuration")
})


test_that("run_covr_modes handles error when unloading package", {
  pkg_source_path <- "mock/path"
  
  # Mock get_pkg_desc to return a known package
  mock_get_pkg_desc <- function(pkg_source_path, fields) {
    list(Package = "mockpkg", Version = "0.1.0")
  }
  
  # Mock unloadNamespace to simulate failure
  mock_unloadNamespace <- function(pkg) {
    stop("Simulated unload failure")
  }
  
  # Stub loadedNamespaces to simulate that the package is loaded
  mock_loadedNamespaces <- function() {
    c("mockpkg", "stats", "utils")
  }
  
  # Stub dependencies
  mockery::stub(run_covr_modes, "get_pkg_desc", mock_get_pkg_desc)
  mockery::stub(run_covr_modes, "unloadNamespace", mock_unloadNamespace)
  mockery::stub(run_covr_modes, "loadedNamespaces", mock_loadedNamespaces)
  mockery::stub(run_covr_modes, "check_pkg_tests_and_snaps", function(...) {
    list(has_testthat = FALSE, has_tests_base = FALSE, has_testit = FALSE)
  })
  
  result <- run_covr_modes(pkg_source_path)
  
  expect_equal(result$total_cov, 0)
  expect_match(result$res_cov$errors, "No recognised standard or non-standard testing configuration")
})
