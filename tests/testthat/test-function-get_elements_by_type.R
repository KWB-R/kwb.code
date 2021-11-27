test_that("get_elements_by_type() works", {

  f <- function(...) kwb.code:::get_elements_by_type(..., dbg = FALSE)
  
  expect_error(f())
  
  x <- parse(text = "square <- function(x) x * x")
  
  result <- f(x)
  
  expect_type(result, "list")
  expect_true("language|call|<-|3|" %in% names(result))
})
