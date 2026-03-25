#' get skipped tests with line locations
#'
#' @param problem_tests - problem tests
#' @param test_map - all tests with line locations
#'
#' @return test_skip
#' @keywords internal
get_tests_skip_stf <- function(problem_tests, test_map) {
  
  # check for 'problem_tests' data frame
  checkmate::assert_data_frame(problem_tests, any.missing = TRUE, min.rows = 1)
  checkmate::assert_names(names(problem_tests), must.include = c("file", "context"))
  
  # check for 'test_map' data frame
  checkmate::assert_data_frame(test_map, any.missing = FALSE, min.rows = 1)
  checkmate::assert_names(names(test_map), must.include = c("file", "context"))
  
  
  # Filter test_map to only include rows matching both context and file in problem_tests
  test_map_filtered <- test_map[
    test_map$context %in% problem_tests$context &
      test_map$file %in% problem_tests$file,
  ]
  
  # Perform the join safely
  test_skip <- dplyr::left_join(problem_tests, test_map_filtered, by = c("context", "file"))
  
  return(test_skip)
}

