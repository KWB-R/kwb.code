test_that("fetch_string_constants_1() works", {

  f <- kwb.code:::fetch_string_constants_1
  
  expect_error(f())

  root <- if ("tests" %in% dir()) "./tests/testthat/" else "."

  capture.output(tree <- kwb.code::parse_scripts(root))
  
  result <- f(tree)
  
  expect_true(is.list(result))
  expect_true("test-function-fetch_string_constants_1.R" %in% names(result))
  expect_true(all(sapply(result, mode) == "character"))
})
