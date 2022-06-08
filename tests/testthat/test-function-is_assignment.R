test_that("is_assignment() works", {

  f <- kwb.code:::is_assignment
  
  expect_error(f())

  parsed <- function(text) parse(text = text)[[1L]]
  
  expect_true(f(parsed("a <- function(x) 1")))
  expect_true(f(parsed("a <- sum(1:10)")))
  expect_true(f(parsed("a <- 1 + 3 * 7")))
  expect_true(f(parsed("a = b(x)")))
  expect_false(f(parsed("b(x)")))
})
