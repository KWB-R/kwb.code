test_that("replaceNames() works", {

  f <- kwb.code:::replaceNames
  
  expect_error(f())

  expr1 <- quote(f1 <- function(a, b) print(a + b))
  expr2 <- quote(x <- 1)
  expr3 <- quote({
    f1 <- function(x) x + 1L # function assignment
    print(f1) # call
    f1 # name (?)
  })
  
  f(expr1)
  f(expr2)
  f(expr3)
  f(expr3, keyvals = list(f1 = "f1_new"))
})
