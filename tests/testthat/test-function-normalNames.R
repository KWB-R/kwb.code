test_that("normalNames() works", {

  f <- kwb.code:::normalNames
  
  expect_error(f())

  expect_identical(f(1, version = 1), "arg_1")
  expect_identical(f(1, version = 2), ".A.")
  expect_identical(f(1, version = 3), ".a.")
})
