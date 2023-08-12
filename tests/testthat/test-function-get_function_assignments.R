test_that("get_function_assignments() works", {

  f <- kwb.code::get_function_assignments

  expect_error(f())

  file <- tempfile("test-", fileext = ".R")

  writeLines(
    text = c(
      "id <- function(x) x",
      "plus <- function(x, y) x + y"
    ),
    con = file
  )

  result <- f(file)

  expect_identical(names(result), c("id", "plus"))
})
