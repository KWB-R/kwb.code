test_that("vector_to_count_table() works", {

  f <- kwb.code:::vector_to_count_table

  expect_error(f())

  expect_null(f(character()))

  result <- f(c("x", "x", "y", "x", "y"))
  
  expect_identical(
    result, 
    kwb.utils::noFactorDataFrame(
      name = c("x", "y"), 
      count = c(3L, 2L)
    )
  )
})
