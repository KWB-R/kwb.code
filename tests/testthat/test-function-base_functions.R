test_that("base_functions() works", {

  result <- kwb.code:::base_functions()

  expect_true(all(c("+", "-", "*", "/") %in% result))
})
