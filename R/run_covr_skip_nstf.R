#' Run Coverage Analysis on Package Source and Tests
#'
#' This internal function maps source files to test files, checks for skipped/problematic tests,
#' runs the remaining test files manually, calculates coverage, and optionally generates a report.
#'
#' @param pkg_source_path Character string. Path to the root of the package source directory.
#' @param test_pkg_data List. Output from `check_pkg_tests_and_snaps()` indicating test framework presence.
#'
#'
#' @return returns a list with coverage object and source files with no tests.
#'
#' @examples
#' \dontrun{
#' r = getOption("repos")
#' r["CRAN"] = "http://cran.us.r-project.org"
#' options(repos = r)
#' 
#' dp <- system.file("test-data", 
#'   "MASS_7.3-65.tar.gz", 
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
#' test_covr_list <- run_covr_skip_nstf(pkg_source_path, test_pkg_data)
#'
#' options(old)
#' }
#' @export
run_covr_skip_nstf <- function(pkg_source_path, test_pkg_data) {
  
  cov_env <- new.env(parent = globalenv())
  
  pkg_name <- extract_package_name(pkg_source_path)
  
  # set up path to test directory 
  # convert fs_path to character string for test_dir
  path <- as.character(pkg_source_path)
  testdir <- file.path(path, "tests")
  
  test_path <- get_test_path(test_pkg_data, testdir)
  
  test_path <- normalizePath(test_path)
  
  message(glue::glue("performing source test mapping for {pkg_name}"))
  mapping <- tryCatch({
    get_source_test_mapping_nstf(pkg_source_path, test_path)
  }, error = function(e) {
    cleanup_and_return_null(glue::glue("Error in source-test mapping for {pkg_name}: {e$message}"))
  })
  if (is.null(mapping)) return(NULL)
  
  message(glue::glue("identifying problem tests for {pkg_name}"))
  
  # check for testit testing framework
  if (isTRUE(test_pkg_data$has_testit)) {
    problems <- tryCatch({
      check_covr_skip_testit(pkg_name, mapping, test_path)
    }, error = function(e) {
      cleanup_and_return_null(glue::glue("Error identifying skipped/problematic tests for {pkg_name}: {e$message}"))
    })
  } else {
    problems <- tryCatch({
      check_covr_skip_nstf(pkg_source_path, mapping)
    }, error = function(e) {
      cleanup_and_return_null(glue::glue("Error identifying skipped/problematic tests for {pkg_name}: {e$message}"))
    })
  }
  if (is.null(problems)) return(NULL)
  
  # check for tests to skip
  skip_tests <- problems$test_file[
    !is.na(problems$test_file) &
      !startsWith(problems$issue_type, "warning:") &
      problems$issue_type != "No tests skipped"
  ]
  
  # create df with source files with no tests
  no_tests_df <- get_function_no_tests(mapping) 
  
  # remove source files with no tests
  clean_mapping <- if (anyNA(mapping)) tidyr::drop_na(mapping) else mapping
  
  # create test files clean with testit testing framework and tests testing framework
  if (isTRUE(test_pkg_data$has_testit)) {
    test_files_clean <- file.path(pkg_source_path, "tests/testit", clean_mapping$test_file)
    if (!is.null(skip_tests)| (length(skip_tests) > 0)) {
      test_files_clean <- setdiff(test_files_clean, file.path(pkg_source_path, "tests/testit", skip_tests))
    }
  } else {
    test_files_clean <- file.path(pkg_source_path, "tests", clean_mapping$test_file)
    if (!is.null(skip_tests)| (length(skip_tests) > 0)) {
      test_files_clean <- setdiff(test_files_clean, file.path(pkg_source_path, "tests", skip_tests))
    }
  }
  
  message(glue::glue("loading package and datasets for {pkg_name}"))
  tryCatch({
    pkgload::load_all(pkg_source_path)
    
    pkg_all <- ls(envir = asNamespace(pkg_name), all.names = TRUE)
    
    for (obj in pkg_all) {
      tryCatch({
        assign(obj, get(obj, envir = asNamespace(pkg_name)), envir = cov_env)
      }, error = function(e) {
        message(glue::glue("Failed to assign object {obj}: {e$message}"))
      })
    }
    
    
    datasets <- load_package_datasets(pkg_name, env = cov_env)
    message("Datasets loaded: ", paste(datasets, collapse = ", "))
  }, error = function(e) {
    cleanup_and_return_null(glue::glue("Error loading package or datasets for {pkg_name}: {e$message}"))
  })
  
  message(glue::glue("running coverage manually for {pkg_name}"))
  
  coverage <- tryCatch({
    covr::environment_coverage(
      env = cov_env,
      test_files = test_files_clean
    )
  }, error = function(e) {
    cleanup_and_return_null(glue::glue("Error running coverage for {pkg_name}: {e$message}"), env = cov_env)
  })
  
  
  if (!is.null(coverage)) {
    
    # create coverage list
    res_cov <- tryCatch({
      coverage_list <- covr::coverage_to_list(coverage)
      list(name = pkg_name, coverage = coverage_list, errors = NA, notes = NA)
    },
    error = function(cond){
      coverage_list <- list(filecoverage = NA, totalcoverage = NA_integer_)
      list(
        name = pkg_name, coverage = coverage_list,
        errors = cond,
        notes = NA
      )
    })
    
    total_cov <- as.numeric(res_cov$coverage$totalcoverage/100)
    percent_cov <- as.numeric(res_cov$coverage$totalcoverage)
    
    percent_cov_round <- round(percent_cov, 2)
    message(glue::glue("Total Coverage for {pkg_name} is: {percent_cov_round} %"))
    
    cleanup_and_return_null(env = cov_env)
    
    # structure the return list
    covr_list <- list(
      total_cov = total_cov,
      res_cov = list(
        name = res_cov$name,
        coverage = res_cov$coverage,
        errors = res_cov$errors,
        notes = res_cov$notes
      ),
      functions_no_tests = no_tests_df,
      tests_skipped = skip_tests,
      tests_passing = test_files_clean
    )
    
  } else {
    message("No test coverrage for this configuration")
    covr_list <- list(
      total_cov = 0,
      res_cov = list(
        name = pkg_name,
        coverage = list(
          filecoverage = matrix(0, nrow = 1, dimnames = list("No functions tested")),
          totalcoverage = 0
        ),
        errors = "No testthat or testit configuration",
        notes = NA
      )
    )
    
  } 
  return(covr_list)
}

#' Check for skipped or errored test files (non-testit)
#'
#' This internal function attempts to run test files mapped to source files and
#' identifies any that produce warnings or errors. It does not handle `testit`.
#'
#' @param pkg_source_path Path to the root of the package source.
#' @param mapping A data frame with a column `test_file` listing test file names.
#'
#' @return A data frame listing test files and any issues encountered.
#' @keywords internal
check_covr_skip_nstf <- function(pkg_source_path, mapping) {
  
  test_dir <- file.path(pkg_source_path, "tests")
  
  clean_mapping <- if (anyNA(mapping)) tidyr::drop_na(mapping) else mapping
  
  problems <- data.frame(test_file = character(), issue_type = character(), stringsAsFactors = FALSE)
  
  problem_list <- lapply(clean_mapping$test_file, function(test) {
    test_path <- file.path(test_dir, test)
    
    result <- tryCatch(
      {
        suppressMessages(
          suppressWarnings(
            capture.output(
              source(test_path, local = new.env()),
              file = NULL
            )
          )
        )
        NULL
      },
      warning = function(w) "warning",
      error = function(e) "error"
    )
    
    if (!is.null(result)) {
      return(data.frame(test_file = test, issue_type = result, stringsAsFactors = FALSE))
    } else {
      return(NULL)
    }
  })
  
  problems <- do.call(rbind, problem_list)
  
  if (is.null(problems) || nrow(problems) == 0) {
    problems <- data.frame(
      test_file = NA_character_,
      issue_type = "No tests skipped",
      stringsAsFactors = FALSE
    )
  }
  
  return(problems)
}

#' Run testit tests individually, capturing all warnings and errors, with dynamic package root detection
#'
#' @param pkg_name The package name (not used in this function, but kept for compatibility)
#' @param mapping A data frame with a column `test_file` listing test file names
#' @param test_path The path to the test directory (e.g., tests/testit)
#'
#' @return A data frame with test_file and issue_type columns
#' @keywords internal
#' @noRd
check_covr_skip_testit <- function(pkg_name, mapping, test_path) {
  
  # Helper to find the package root by searching for DESCRIPTION upward
  find_pkg_root <- function(start_path) {
    current <- normalizePath(start_path, mustWork = TRUE)
    while (!file.exists(file.path(current, "DESCRIPTION"))) {
      parent <- dirname(current)
      if (parent == current) stop("Package root (with DESCRIPTION) not found.")
      current <- parent
    }
    current
  }
  
  pkg_root <- find_pkg_root(test_path)
  
  # Prepare mapping
  clean_mapping <- if (anyNA(mapping)) tidyr::drop_na(mapping) else mapping
  
  problems <- data.frame(test_file = character(), issue_type = character(), stringsAsFactors = FALSE)
  
  problem_list <- lapply(clean_mapping$test_file, function(test) {
    test_file_path <- file.path(test_path, test)
    # Set working directory to package root for each test
    original_wd <- getwd()
    setwd(pkg_root)
    on.exit(setwd(original_wd), add = TRUE)
    
    result <- tryCatch(
      {
        capture.output(
          source(test_file_path, local = new.env()),
          file = NULL
        )
        NULL
      },
      warning = function(w) paste("warning:", conditionMessage(w)),
      error = function(e) paste("error:", conditionMessage(e))
    )
    
    if (!is.null(result)) {
      data.frame(test_file = test, issue_type = result, stringsAsFactors = FALSE)
    } else {
      NULL
    }
  })
  
  problems <- do.call(rbind, problem_list)
  
  if (is.null(problems) || nrow(problems) == 0) {
    problems <- data.frame(
      test_file = NA_character_,
      issue_type = "No tests skipped",
      stringsAsFactors = FALSE
    )
  }
  
  return(problems)
}


#' Map R Source Files to Corresponding Test Files
#'
#' This internal helper function scans the `R/` and `tests/` directories of a package
#' and attempts to match each source file with a corresponding test file based on filename similarity.
#'
#' @param pkg_source_path Character string. Path to the root of the package source directory.
#' @param test_dir character string. directory where tests are
#'
#' @return A data frame with two columns:
#' \describe{
#'   \item{source_file}{Name of the source file in the `R/` directory.}
#'   \item{test_file}{Name of the matched test file in the `tests/` directory, or \code{NA} if no match is found.}
#' }
#'
#' @keywords internal
get_source_test_mapping_nstf <- function(pkg_source_path, test_dir) {
  source_dir <- file.path(pkg_source_path, "R")
  # test_dir <- file.path(pkg_source_path, "tests")
  
  src_files <- list.files(source_dir, pattern = "\\.R$", full.names = FALSE)
  test_files <- list.files(test_dir, pattern = "\\.R$", full.names = FALSE)
  
  mapping <- do.call(rbind, lapply(tools::file_path_sans_ext(src_files), function(src) {
    match <- test_files[grepl(src, tools::file_path_sans_ext(test_files), ignore.case = TRUE)]
    data.frame(
      source_file = paste0(src, ".R"),
      test_file = ifelse(length(match) == 0, NA, match[1]),
      stringsAsFactors = FALSE
    )
  }))
  
  return(mapping)
}