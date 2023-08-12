#library(testthat)
test_that("type_info() works", {

  f <- kwb.code:::type_info
  
  expect_error(f())

  check_result <- function(x) {
    expect_type(x, "list")
    expect_identical(names(x), c(
      "type", 
      "mode", 
      "class", 
      "length", 
      "text", 
      "is",
      "n_modes", 
      "n_classes"
    ))
  }
  
  check_result(result <- f(1))

  check_result(result <- f(list(a = 1, b = 2)))
  expect_identical(result$length, 2L)
})
