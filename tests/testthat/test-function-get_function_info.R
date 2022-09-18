test_that("get_function_info() works", {

  f <- kwb.code:::get_function_info
  
  expect_error(f())

  result <- f(quote(f1 <- function(x) x))
  
  expect_s3_class(result, "data.frame")
  expect_true(nrow(result) == 1L)
  expect_identical(result$functionName, "f1")
})
