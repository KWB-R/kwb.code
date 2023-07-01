test_that("expressions_by_class() works", {

  f <- kwb.code:::expressions_by_class
  
  expect_error(f())

  expect_null(f(character(0)))
  expect_null(f(list(a = 1, b = 2)))
})
