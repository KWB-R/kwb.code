test_that("is_what() works", {
  
  f <- kwb.code:::is_what
  
  check <- function(x) {
    result_1 <- f(x, dbg = FALSE)
    expect_output(result_2 <- f(x, dbg = TRUE))
    expect_identical(result_1, result_2)
    expect_true(all(sapply(
      X = paste0("is.", result_1), 
      FUN = function(name) do.call(name, list(x))
    )))
  }

  check(1L)
  check(1)
  check("a")
})
