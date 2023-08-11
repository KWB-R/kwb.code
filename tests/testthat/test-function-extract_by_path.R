#library(testthat)

test_that("extract_by_path() works", {

  f <- kwb.code:::extract_by_path
  
  expect_error(f())
  expect_error(f("a"))
  
  x <- list(
    list( # [[1]]
      11, 
      12
    ),
    list( # [[2]]
      21,
      22,
      list(
        231,
        232
      )
    )
  )
  
  expect_identical(f(x, "1"), x[1L])
  expect_identical(f(x, c("1", "2")), x[c(1, 2)])
  expect_identical(f(x, c("/1/1", "2/2", "/2/3/2")), list(11, 22, 232))
})
