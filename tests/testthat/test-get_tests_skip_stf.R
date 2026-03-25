test_that("get_tests_skip_stf joins problem_tests with test_map correctly", {
  problem_tests <- data.frame(
    file = c("test-a.R", "test-b.R"),
    context = c("context1", "context2"),
    stringsAsFactors = FALSE
  )
  
  test_map <- data.frame(
    file = c("test-a.R", "test-b.R", "test-c.R"),
    context = c("context1", "context2", "context3"),
    line = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  
  mockery::stub(get_tests_skip_stf, "checkmate::assert_data_frame", NULL)
  mockery::stub(get_tests_skip_stf, "checkmate::assert_names", NULL)
  
  result <- get_tests_skip_stf(problem_tests, test_map)
  
  expect_equal(nrow(result), 2)
  expect_equal(result$line, c(10, 20))
})

