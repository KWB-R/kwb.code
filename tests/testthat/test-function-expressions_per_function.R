test_that("expressions_per_function() works", {

  f <- kwb.code:::expressions_per_function
  
  expect_error(f())

  result <- f(data.frame(script = "s1", n.expr = 1))
  
  expect_s3_class(result, "data.frame")
  expect_identical(nrow(result), 1L)
  expect_identical(names(result), c("script", "n.expr.sum", "n.expr.n", "epf"))
})

