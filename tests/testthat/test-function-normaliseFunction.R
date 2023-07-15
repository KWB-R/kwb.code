test_that("normaliseFunction() works", {

  f <- kwb.code:::normaliseFunction

  assignment <- quote(f1 <- function(x) x + 1L)
  
  expect_error(f())

  result <- f(assignment)
  
  expect_true(is.language(result))
  
  expect_true(grepl("\\.A\\.", as.character(result)[3L]))
})
