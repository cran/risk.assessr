#' Run covr with failing tests
#' #' 
#' This function runs test coverage and skip analysis for a package using testthat or other supported frameworks.
#'
#' @param pkg_source_path Character. Path to the root of the package source.
#' @param test_pkg_data List. Output from `check_pkg_tests_and_snaps()` indicating test framework presence.
#'
#' @return skip_results - test coverage with PASS tests passing or FAIL for tests failing
#' 
#' @examples
#' \dontrun{
#' r = getOption("repos")
#' r["CRAN"] = "http://cran.us.r-project.org"
#' old <- options(repos = r)
#'
#' dp <- system.file("test-data", 
#'   "test.package.0014_0.1.0.tar.gz", 
#'   package = "risk.assessr")
#' 
#' # set up package
#' install_list <- set_up_pkg(dp)
#'
#' build_vignettes <- install_list$build_vignettes
#' package_installed <- install_list$package_installed
#' pkg_source_path <- install_list$pkg_source_path
#' rcmdcheck_args <- install_list$rcmdcheck_args
#'
#' # install package locally to ensure test works
#' package_installed <- install_package_local(pkg_source_path)
#' package_installed <- TRUE
#'
#' rcmdcheck_args$path <- pkg_source_path
#' test_pkg_data <- check_pkg_tests_and_snaps(pkg_source_path)
#' test_covr_list <- run_covr_skip_stf(pkg_source_path, test_pkg_data)
#' 
#' options(old)
#' }  
#' @export
run_covr_skip_stf <- function(pkg_source_path, test_pkg_data) {
  
  # Input checking
  checkmate::assert_string(pkg_source_path)
  checkmate::assert_directory_exists(pkg_source_path)
  
  # set up path to test directory 
  # convert fs_path to character string for test_dir
  path <- as.character(pkg_source_path)
  testdir <- file.path(path, "tests")
  
  pkg <- get_pkg_name(pkg_source_path)
  
  test_path <- get_test_path(test_pkg_data, testdir)
  
  if (is.null(test_path))  {
    message(glue::glue("No test directory found for {pkg}"))
    skip_results <- list(
      res_cov = NA,
      long_summary = NA,
      test_skip = NA
    )
    return(skip_results)
  }
  
  # set path as working directory in the test env
  setwd(path)
  
  message(glue::glue("Creating Default test reporter for {pkg}"))
  
  # set max fails to infinite
  testthat::set_max_fails(Inf)
  
  # default reporter
  sink(tempfile())
  test_results <- testthat::test_dir(
    path = test_path,
    reporter = testthat::default_reporter(),
    stop_on_failure = FALSE,
    stop_on_warning = FALSE,
    load_package = "source"
  )
  sink()  # Reset output
  message(glue::glue("Mapping package test structure for {pkg}"))
  
  # create path to test folder for test map
  map_path <- test_path
  
  # Map the hierarchy structure of a testthat test file
  # Query a test file to see what expectations are defined in it.
  test_map <- map_tests_stf(map_path)
  
  # create long summary
  test_results_long <- 
    get_tests_long_sum_stf(test_results)
  
  #rename columns for aligning in join
  lookup <- c(code_script = "context", context = "test")
  
  test_results_long <- dplyr::rename(test_results_long, all_of(lookup))
  
  # Extract failed/skipped expectations only
  test_results_filtered <- test_results_long[test_results_long$status != 'PASS', ]
 
  # Clean up file names
  test_results_filtered$file <- gsub('#(.*?)$', '', basename(test_results_filtered$file))
  
  # align column names in test map
  test_map <- test_map |> 
    dplyr::rename(context = "test")
  
  
  # Filter tests with status FAILED, ERROR, or SKIPPED
  status_filter <- c("FAILED", "ERROR", "SKIPPED")
  problem_tests <- subset(test_results_filtered, status %in% status_filter)
  
  # create empty df with 1 row if problem tests is empty
  if (nrow(problem_tests) == 0) {
    problem_tests <- data.frame(
      file = "",
      code_script = "",
      context = "",
      status = "",
      n = NA_integer_,
      time = NA_real_,
      stringsAsFactors = FALSE
    )
    
  }
  
  # check for failed tests
  failed <- any(test_results_long$status%in%c('ERROR','FAILED'))
  
  if (failed == TRUE) {
  
    test_skip <- get_tests_skip_stf(problem_tests, test_map)
    
    message(glue::glue("Creating skip test map for {pkg}"))
    
    cvr <- run_covr_safe_stf(pkg, map_path, test_skip, test_map)
    
    message(glue::glue("Creating test coverage for {pkg}"))
    
    res_sum <- create_results_summary(cvr)
    
    # structure the return list
    covr_list <- list(
      total_cov = res_sum$total_cov,
      res_cov = list(
        name = res_sum$res_cov$name,
        coverage = res_sum$res_cov$coverage,
        errors = res_sum$res_cov$errors,
        notes = res_sum$res_cov$notes
      ),
      long_summary = test_results_long,
      test_skip = test_skip
    )
  } else {
    message(glue::glue("Creating test results for no skipped tests for {pkg}"))
    covr_list <- create_covr_list_no_skip(test_map, 
                                          test_results, 
                                          pkg_source_path,
                                          pkg)
  }
  
  return(covr_list)
}

#' Run skip coverage safely in a separate R session
#'
#' This internal helper function uses `callr::r_safe()` to execute the
#' `coverage_skip()` function from the `risk.assessr` package in a clean R session.
#' This avoids issues with loaded namespaces (e.g., `dplyr`) that may interfere with coverage analysis.
#'
#' It includes error handling and a configurable timeout to prevent long-running or stuck processes.
#'
#' @param pkg Character. Name of the package being tested.
#' @param map_path Character. Path to the `testthat` directory containing mapped test structure.
#' @param test_skip Data frame. Skip map generated from problematic tests.
#' @param test_map Data frame. Mapped structure of test files and expectations.
#' @param timeout Numeric. Maximum time (in seconds) allowed for the coverage process to run. Default is 120 seconds.
#'
#' @return A coverage object returned by `coverage_skip()`, or `NULL` if an error occurs or timeout is reached.
#' @keywords internal
run_covr_safe_stf <- function(pkg, map_path, test_skip, test_map, timeout = 120) {
  tryCatch({
    callr::r_safe(
      function(pkg, map_path, test_skip, test_map) {
        # Load the required package inside the safe environment
        library(risk.assessr)
        
        # Run coverage
        create_coverage_skip_stf(
          test_path = map_path,
          test_skip = test_skip,
          test_map = test_map
        )
      },
      args = list(pkg = pkg, map_path = map_path, test_skip = test_skip, test_map = test_map),
      timeout = timeout
    )
  }, error = function(e) {
    message(glue::glue("Error running coverage for {pkg}: {e$message}"))
    return(NULL)
  })
}

#' @title Run covr with failing tests
#' @description Run \code{\link[covr]{package_coverage}} ignoring failing tests from \code{\link[testthat]{test_dir}}.
#' @param test_path character, path to test directory, Default: 'tests/testthat'
#' @param test_skip data.frame, mapping of failing tests, PARAM_DESCRIPTION,
#' @param test_map data.frame, mapping of all the tests in the package
#' @return NULL
#' @details If test_skip is NULL then a new test_dir will be run internally checking
#' for failing tests.
#' @seealso
#'  \code{\link[testthat]{test_dir}}
#'  \code{\link[covr]{package_coverage}}
#' @rdname coverage_skip
#' @family utility
#' @export
#' @importFrom testthat test_dir
#' @importFrom covr package_coverage
create_coverage_skip_stf <- function(
    test_path = "tests/testthat",
    test_skip,
    test_map) {
  
  # Identify lines to skip based on test_skip
  
  test_skip_lines <- lapply(
    split(test_skip, test_skip$file),
    function(x) {
      unlist(mapply(seq, from = x$line1, to = x$line2, SIMPLIFY = FALSE))
    }
  )
  
  # If there are lines to skip, comment them out
  if (length(test_skip_lines) > 0) {
    on.exit({
      # Restore original test files by uncommenting lines
      for (nx in names(test_skip_lines)) {
        file_path <- file.path(test_path, nx)
        test_lines <- readLines(file_path, warn = FALSE)
        lines_to_uncomment <- test_skip_lines[[nx]]
        test_lines[lines_to_uncomment] <- gsub("^#", "", test_lines[lines_to_uncomment])
        cat(test_lines, file = file_path, sep = "\n")
      }
    }, add = TRUE)
    
    # Comment out failing test lines
    for (nx in names(test_skip_lines)) {
      file_path <- file.path(test_path, nx)
      test_lines <- readLines(file_path, warn = FALSE)
      lines_to_comment <- test_skip_lines[[nx]]
      test_lines[lines_to_comment] <- sprintf("#%s", test_lines[lines_to_comment])
      cat(test_lines, file = file_path, sep = "\n")
    }
  }
  
  # Run coverage
  covr::package_coverage()
}

#' Create a results summary for easier access to scores
#' 
#' @description - retrieve overall percentage and file coverage percentages
#' 
#' @param cvr - covr object with full results of skip coverage
#'
#' @return - dataframe with total scores
#'@keywords internal
create_results_summary <- function(cvr) {
  # check for empty cvr object
  if (is.null(cvr)) {
    # create filecoverage object
    filecoverage <- structure(
      c(NA_real_),
      dim = c(1),
      dimnames = list("no_coverage.R")
    )
    # create covr_list object
    covr_list <- list(
      total_cov = NA,
      res_cov = list(
        name = NA,
        coverage = list(
          filecoverage = filecoverage,
          totalcoverage = NA
        ),
        errors = NA,
        notes = NA
      )
    )
    
  } else {
    # Extract package name
    pkg_name <- attr(cvr, "package")$package
    # Extract overall coverage percentage
    total_cov <- covr::percent_coverage(cvr)
    
    # convert percentage into a score between 0 and 1
    total_cov <- total_cov / 100
    
    
    # Aggregate coverage by base filename
    file_coverage <- lapply(names(cvr), function(srcref_name) {
      entries <- cvr[[srcref_name]]
      values <- entries$value
      base_file <- sub(":.*", "", srcref_name)  # Extract base filename before first colon
      data.frame(filename = base_file, covered = values > 0)
    })
    
    file_coverage_df <- do.call(rbind, file_coverage)
    
    # Summarize coverage per base file
    file_summary <- file_coverage_df %>%
      dplyr::group_by(filename) %>%
      dplyr::summarise(
        percent = round(100 * sum(covered) / n(), 2),
        .groups = "drop"
      )
    
    # Create 1D numeric vector with dimnames (correct structure)
    filecoverage <- setNames(file_summary$percent, file_summary$filename)
    filecoverage <- structure(
      filecoverage,
      dim = c(length(filecoverage)),
      dimnames = list(names(filecoverage))
    )
    
    # Build result structure
    covr_list <- list(
      total_cov = total_cov,
      res_cov = list(
        name = pkg_name,
        coverage = list(
          filecoverage = filecoverage,
          totalcoverage = total_cov
        ),
        errors = NA,
        notes = NA
      )
    )
  }
  return(covr_list)
}


#' create covr_list when no tests are skipped
#'
#' @param test_map - R object with mapped tests
#' @param test_results_long - cvr object with no tests skipped
#' @param pkg_source_path - path to the package
#' @param pkg - name of the package
#'
#' @returns - summary cvr object
#' @keywords internal
create_covr_list_no_skip <- function(test_map, 
                                     test_results_long, 
                                     pkg_source_path, 
                                     pkg) {
  
  # Attempt to unload package safely
  tryCatch({
    if (pkg %in% loadedNamespaces()) {
      message(glue::glue("Unloading {pkg} package to avoid coverage conflict..."))
      unloadNamespace(pkg)
    }
  }, error = function(e) {
    message(glue::glue("Warning: Failed to unload {pkg} package. Proceeding anyway."))
    message("Details: ", e$message)
  })
  
  # Run coverage on the package
  message(glue::glue("Creating package coverage for {pkg}"))
  
  coverage <- covr::package_coverage(path = pkg_source_path, type = "tests")
  
  # Enrich each test entry with passed, failed, and skipped counts
  test_results_long <- lapply(test_results_long, function(entry) {
    entry$passed <- sum(sapply(entry$results, function(res) {
      inherits(res, "expectation_success")
    }))
    entry$failed <- sum(sapply(entry$results, function(res) {
      inherits(res, "expectation_failure")
    }))
    entry$skipped <- sum(sapply(entry$results, function(res) {
      inherits(res, "expectation_skip")
    }))
    return(entry)
  })
  
  # Extract coverage list from covr::coverage_to_list
  coverage_list <- covr::coverage_to_list(coverage)
  
  # Extract filecoverage and totalcoverage
  filecoverage <- coverage_list$filecoverage
  totalcoverage <- coverage_list$totalcoverage
  
  # Aggregate passed and failed counts
  total_passed <- sum(sapply(test_results_long, function(entry) entry$passed))
  total_failed <- sum(sapply(test_results_long, function(entry) entry$failed))
  
  # Collect skipped test entries
  tests_skipped <- Filter(function(entry) entry$skipped > 0, test_results_long)
  
  # Construct res_cov
  res_cov <- list(
    name = pkg,
    coverage = list(
      filecoverage = filecoverage,
      totalcoverage = totalcoverage
    ),
    errors = NA,
    notes = NA,
    passed = total_passed,
    failed = total_failed
  )
  
  # Compute total_cov
  total_cov <- if (!is.na(totalcoverage)) totalcoverage / 100 else NA_real_
  
  # Final covr_list
  covr_list <- list(
    total_cov = total_cov,
    res_cov = res_cov,
    tests_skipped = tests_skipped
  )
  
  # remove yarn datset created by pls package
  if (exists("yarn", envir = .GlobalEnv)) {
    rm(yarn, envir = .GlobalEnv)
  }
  
  return(covr_list)
}
