#' @title Summarize detailed testthat results
#' @description Internal helper function to generate a long-format summary of testthat results, including status and line references.
#' @param x list of test result objects
#' @return data.frame with detailed test results including status and source line links
#' @keywords internal
get_tests_long_sum_stf <- function(x) {
  
  # Return NULL if input is empty
  if (length(x) == 0) {
    return(NULL)
  }
  
  # Convert the list of test results to a data.frame
  x1 <- data.frame(x, stringsAsFactors = FALSE)
  
  # Convert logical columns to numeric (e.g., TRUE/FALSE to 1/0)
  for (i in seq(1:ncol(x1)))
    if (is.logical(x1[[i]])) {
      x1[[i]] <- as.numeric(x1[[i]])
    }
  
  # Add a 'pass' column: 1 if no failures, skips, warnings, or errors; 0 otherwise
  x1$pass <- as.numeric((apply(x1[, c("failed", "skipped", "warning", "error")], 1, sum)) == 0)
  
  # Initialize a 'status' column
  x1$status <- NA
  
  # Assign status based on test result conditions
  for (i in 1:nrow(x1)) {
    if (x1$pass[i] == 1) {
      x1$status[i] <- "PASS"
    }
    
    if (x1$failed[i] != 0) {
      x1$status[i] <- "FAILED"
    }
    
    if (x1$error[i] != 0) {
      x1$status[i] <- "ERROR"
    }
    
    if (x1$skipped[i] != 0) {
      x1$status[i] <- "SKIPPED"
    }
    
    if (x1$warning[i] != 0) {
      x1$status[i] <- "WARNING"
    }
  }
  
  # Select and rename relevant columns for output
  ret <- x1[, c("file", "context", "test", "status", "nb", "real")]
  names(ret)[c(5, 6)] <- c("n", "time")
  
  # Generate line references for each test result
  lines <- sapply(x, function(test_result) {
    src_lines <- unique(c(
      x[[1]]$src[1],
      x[[1]]$src[3]
    ))
    src_lines <- src_lines[!is.na(src_lines)]
    paste0("L", src_lines, collapse = "_")
  })
  
  # Combine file name with line references
  ret$file <- sprintf("%s#%s", ret$file, lines)
  
  
  # Return the final long-format summary
  ret
}
