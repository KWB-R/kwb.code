test_that("getNewPairlist() works", {

  f <- kwb.code:::getNewPairlist
  
  expect_error(f())

  result <- f(pairlist(a = 1, b = 2))
  
  expect_true(!is.null(attr(result, "renames")))
  expect_identical(names(result), c(".A.", ".B."))
  
})
