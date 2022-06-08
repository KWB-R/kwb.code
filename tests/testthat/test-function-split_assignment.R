test_that("split_assignment() works", {

  f <- kwb.code:::split_assignment
  
  expect_error(f())
  
  expect_identical(
    f(parse(text = "x <- 1")[[1L]]), 
    list(leftSide = substitute(x), rightSide = 1)
  )
  
  expect_identical(
    f(parse(text = "x <- a + b")[[1L]]), 
    list(leftSide = substitute(x), rightSide = substitute(a + b))
  )
})
