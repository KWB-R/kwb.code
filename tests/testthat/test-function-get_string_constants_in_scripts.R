test_that("get_string_constants_in_scripts() works", {

  f <- kwb.code:::get_string_constants_in_scripts
  
  expect_error(f())

  root <- if ("tests" %in% dir()) "./tests/testthat/" else getwd()
  
  capture.output(result1 <- f(root))
  
  scripts <- "test-function-get_string_constants_in_scripts.R"
  
  capture.output(result1 <- f(root, scripts))
  capture.output(result2 <- f(root, scripts, two_version_check = FALSE))
  
  expect_s3_class(result1, "data.frame")
  
  expect_identical(names(result1), c("file_id", "string", "count"))
  
  expect_identical(result1, result2)
})
