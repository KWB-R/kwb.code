test_that("find_string_constants() works", {
  
  f <- kwb.code:::find_string_constants
  
  root <- if ("tests" %in% dir()) "./tests/testthat/" else getwd()
  
  capture.output(result <- f(root))

  expect_s3_class(result, "data.frame")
  
})
