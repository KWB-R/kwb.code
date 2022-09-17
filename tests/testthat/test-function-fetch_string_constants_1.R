test_that("fetch_string_constants_1() works", {

  f <- kwb.code:::fetch_string_constants_1
  
  expect_error(f())

  capture.output(tree <- kwb.code::parse_scripts("./R"))
  
  result <- f(tree)
  
  expect_true(is.list(result))
  expect_true("utils.R" %in% names(result))
  expect_true(all(sapply(result, mode) == "character"))
})
