#' Run Coverage Analysis with Test Detection
#'
#' This function checks the test configuration of an R package and runs code coverage analysis
#' using `testthat`, `testit` or `test base` frameworks if available. If no supported test framework is found,
#' it returns a default coverage result indicating no functions were tested.
#'
#' @param pkg_source_path Character. Path to the root directory of the R package source.
#' @param covr_timeout Numeric. Timeout in seconds for running coverage analysis. Default is 60.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{covr_list}{A list containing coverage results. If no supported tests are found, returns a default structure with zero coverage.}
#'   \item{test_pkg_data}{A list summarizing the test configuration, including presence of `testthat`, `testit`, snapshots, and base R test scripts.}
#' }
#'
#' @examples
#' \dontrun{
#' r = getOption("repos")
#' r["CRAN"] = "http://cran.us.r-project.org"
#' old <- options(repos = r)
#'
#' dp <- system.file("test-data", 
#'   "test.package.0001_0.1.0.tar.gz", 
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
#' covr_mode_list <- run_covr_modes(pkg_source_path)
#' }
#'
#' @export
run_covr_modes <- function(pkg_source_path,
                           covr_timeout = 60) {
 
  # Get package name and version
  pkg_desc <- get_pkg_desc(pkg_source_path, 
                           fields = c("Package", 
                                      "Version"))
  pkg <- pkg_desc$Package
  
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
  
  test_pkg_data <- check_pkg_tests_and_snaps(pkg_source_path)
  
  if (test_pkg_data$has_testthat | test_pkg_data$has_testit) {
    message(glue::glue("running standard testing framework"))
    covr_list <- run_coverage(
      pkg_source_path,
      covr_timeout
    )
    
    if (is.na(covr_list$total_cov) && all(is.na(covr_list$res_cov$coverage$filecoverage))) {
      
      if (test_pkg_data$has_testthat) {
        message(glue::glue("running skip testing with standard testing framework"))
        covr_list <- run_covr_skip_stf(pkg_source_path, test_pkg_data)
        
      } else if (test_pkg_data$has_testit) {
        message(glue::glue("running skip testing with non-standard testing framework"))
        covr_list <- run_covr_skip_nstf(pkg_source_path, test_pkg_data)
      }
    }
    
  } else if (test_pkg_data$has_tests_base) {
    message(glue::glue("running non-standard testing framework"))
    covr_list <- run_covr_skip_nstf(pkg_source_path, test_pkg_data)
  } else {
    message("No recognised standard or non-standard testing configuration")
    pkg_name <- basename(pkg_source_path)
    covr_list <- list(
      total_cov = 0,
      res_cov = list(
        name = pkg_name,
        coverage = list(
          filecoverage = matrix(0, nrow = 1, dimnames = list("No functions tested")),
          totalcoverage = 0
        ),
        errors = "No recognised standard or non-standard testing configuration",
        notes = NA
      )
    )
  }
  return(covr_list)
}

#' @title Load Datasets from a Package
#' @description 
#' Loads all datasets from a specified installed package into the global environment.
#' It first attempts to load `.rda` files from the package's `data/` directory.
#' If none are found, it falls back to using `data()` to load datasets listed in the package metadata.
#'
#' @param pkg_name A character string specifying the name of the package.
#' @param env A character string specifying the covr env
#'
#' @return A character vector of dataset names that were attempted to be loaded.
#' Returns `NULL` if the package is not installed.
#'
#' @details 
#' This function is intended for internal use. It silently attempts to load each dataset
#' and suppresses errors if loading fails. Datasets are loaded into the global environment.
#'
#' @keywords internal
load_package_datasets <- function(pkg_name, env = cov_env) {
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    message(paste("Package", pkg_name, "is not installed."))
    return(NULL)
  }
  
  pkg_path <- find.package(pkg_name)
  data_path <- file.path(pkg_path, "data")
  datasets <- character()
  
  if (dir.exists(data_path)) {
    files <- list.files(data_path, full.names = TRUE)
    for (file in files) {
      ext <- tools::file_ext(file)
      name <- tools::file_path_sans_ext(basename(file))
      tryCatch({
        if (ext == "rda") {
          load(file, envir = env)
        } else if (ext %in% c("tab", "tsv")) {
          assign(name, read.table(file, header = TRUE, sep = "\t"), envir = env)
        } else if (ext == "csv") {
          assign(name, read.csv(file, header = TRUE), envir = env)
        } else if (ext == "txt") {
          assign(name, read.table(file, header = TRUE), envir = env)
        } else if (ext == "json") {
          if (!requireNamespace("jsonlite", quietly = TRUE)) stop("jsonlite package required")
          assign(name, jsonlite::fromJSON(file), envir = env)
        } else if (ext == "R") {
          sys.source(file, envir = env)
        }
        datasets <- c(datasets, name)
      }, error = function(e) {
        message(paste("Failed to load", file, ":", e$message))
      })
    }
  }
  
  if (length(datasets) == 0) {
    available <- data(package = pkg_name)$results[, "Item"]
    for (dataset in available) {
      tryCatch({
        data(list = dataset, package = pkg_name, envir = env)
        datasets <- c(datasets, dataset)
      }, error = function(e) {
        message(paste("Failed to load dataset", dataset, ":", e$message))
      })
    }
  }
  
  return(unique(datasets))
}


#' Clean up global environment and return NULL
#'
#' This internal utility function is used to clean up the global environment
#' by removing all objects and optionally displaying a message before returning `NULL`.
#' It is primarily used within `run_coverage_base()` to ensure consistent cleanup
#' in error-handling scenarios.
#'
#' @param msg Optional character string. If provided, the message will be printed before cleanup.
#' @param env A character string specifying the covr env
#'
#' @return NULL (invisibly)
#'
#' @keywords internal
cleanup_and_return_null <- function(msg = NULL, env = cov_env) {
  if (!is.null(msg)) message(msg)
  
  rm(list = ls(envir = env), envir = env)
  
  find_project_root <- function(start = getwd()) {
    current <- normalizePath(start)
    while (TRUE) {
      if (file.exists(file.path(current, ".Rproj")) ||
          file.exists(file.path(current, "DESCRIPTION")) ||
          file.exists(file.path(current, ".git"))) {
        return(current)
      }
      parent <- dirname(current)
      if (parent == current) break
      current <- parent
    }
    return(NULL)
  }
  
  root_dir <- find_project_root()
  if (is.null(root_dir)) {
    message("Project root not found.")
    return(NULL)
  }
  
  files_to_remove <- c("polr.pdf", "silhouette-ex.ps")
  invisible(lapply(files_to_remove, function(file) {
    file_path <- file.path(root_dir, file)
    if (file.exists(file_path)) {
      try(file.remove(file_path), silent = TRUE)
    }
  }))
  
  return(NULL)
}


#' get functions with no tests
#'
#' @param mapping_df - data frame with source files and test files
#'
#' @return no_tests_df - data frame with source files with no tests
#' 
#' @keywords internal
get_function_no_tests <- function(mapping_df) {
  # Filter rows where test_file is NA
  no_tests_df <- subset(mapping_df, is.na(test_file))
  
  # Check if filtered_df is empty
  # If no rows are filtered, create a single-row data frame with the comment
  if (nrow(no_tests_df) == 0) {
    no_tests_df <- data.frame(
      source_file = NA,
      test_file = NA,
      comment = "all functions have at least 1 test",
      stringsAsFactors = FALSE
    )
  } else {
    # Add a comment to the filtered rows
    no_tests_df$comment <- "No tests found"
  }
  
  return(no_tests_df)
}