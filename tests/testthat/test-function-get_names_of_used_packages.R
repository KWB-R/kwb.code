test_that("get_names_of_used_packages() works", {

  f <- kwb.code:::get_names_of_used_packages
  
  expect_error(f())

  capture.output(result <- f(root_dir = "./R"))
  
  expect_identical(result, character(0))
})
