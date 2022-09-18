test_that("print_elements_with_modes() works", {

  f <- kwb.code:::print_elements_with_modes
  
  expect_error(f())

  expect_output(f(1))
  expect_output(f(list("a",1, list(2))))
})
