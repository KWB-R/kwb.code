test_that("split_for() works", {

  f <- kwb.code:::split_for
  
  expect_error(f())

  expect_identical(
    f(parse(text = "for (i in 1:10) print(i)")[[1L]]),
    list(
      variable = substitute(i),
      values = substitute(1:10),
      body = substitute(print(i))
    )
  )
})
