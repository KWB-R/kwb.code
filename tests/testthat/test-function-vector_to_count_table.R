test_that("vector_to_count_table() works", {

  f <- kwb.code:::vector_to_count_table

  expect_error(f())

  expect_null(f(character()))

  f(c("x", "x", "y", "x", "y"))
})
