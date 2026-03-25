test_that("get_tests_long_sum_stf returns NULL for empty input", {
  expect_null(get_tests_long_sum_stf(list()))
})

test_that("get_tests_long_sum_stf assigns PASS when no issues", {
  
  # Minimal mock test result structure
  mock_test_result <- list(
    file = "test-mock.R",
    context = "mock context",
    test = "test 1",
    failed = FALSE,
    skipped = FALSE,
    warning = FALSE,
    error = FALSE,
    nb = 1,
    real = 0.01,
    src = c(5, 3, 5, 30, 3, 30, 15, 15)
  )
  
  input <- list(mock_test_result)
  result <- get_tests_long_sum_stf(input)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 8)
})


test_that("get_tests_long_sum_stf assigns FAIL when no issues", {
  
  mock_test_result <- list(
    file = "test-mock_fail.R",
    context = "mock context",
    test = "test fail",
    failed = TRUE,
    skipped = FALSE,
    warning = FALSE,
    error = FALSE,
    nb = 1,
    real = 0.01,
    src = c(5, NA, 15)
  )
  
  input <- list(mock_test_result)
  result <- get_tests_long_sum_stf(input)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("get_tests_long_sum_stf assigns ERROR when no issues", {
  
  mock_test_result <- list(
    file = "test-mock_error.R",
    context = "mock context",
    test = "test error",
    failed = FALSE,
    skipped = FALSE,
    warning = FALSE,
    error = TRUE,
    nb = 1,
    real = 0.01,
    src = c(20, NA, 35)
  )
  
  input <- list(mock_test_result)
  result <- get_tests_long_sum_stf(input)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("get_tests_long_sum_stf assigns SKIPPED when no issues", {
  
  mock_test_result <- list(
    file = "test-mock_skipped.R",
    context = "mock context",
    test = "test skipped",
    failed = FALSE,
    skipped = TRUE,
    warning = FALSE,
    error = TRUE,
    nb = 1,
    real = 0.01,
    src = c(20, NA, 35)
  )
  
  input <- list(mock_test_result)
  result <- get_tests_long_sum_stf(input)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("get_tests_long_sum_stf assigns WARNING when no issues", {
  
  mock_test_result <- list(
    file = "test-mock_warn.R",
    context = "mock context",
    test = "test warning",
    failed = FALSE,
    skipped = TRUE,
    warning = TRUE,
    error = TRUE,
    nb = 1,
    real = 0.01,
    src = c(20, NA, 35)
  )
  
  input <- list(mock_test_result)
  result <- get_tests_long_sum_stf(input)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})