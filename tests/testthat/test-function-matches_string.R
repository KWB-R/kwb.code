test_that("matches_string() works", {

  f <- kwb.code:::matches_string
  
  expect_error(f())

  result <- f("a")
  
  expect_true(result)
  expect_identical(attr(result, "name"), "a")
  
  expect_false(f(1))
})
