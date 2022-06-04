test_that("is_function_assignment() works", {

  f <- kwb.code:::is_function_assignment
  
  expect_error(f())

  parsed <- function(text) parse(text = text)[[1L]]
  
  expect_true(f(parsed("a <- function(x) 1")))
  expect_false(f(parsed("a <- sum(1:10)")))
})
