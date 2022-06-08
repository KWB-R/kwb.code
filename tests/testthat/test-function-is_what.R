test_that("is_what() works", {

  f <- function(...) {
    capture.output(result <- kwb.code:::is_what(..., silent = TRUE))
    result
  }
  
  check <- function(x) {
    result <- f(x)
    expect_true(all(
      sapply(paste0("is.", result), function(name) do.call(name, list(x)))
    ))
  }
  
  check(1L)
  check(1)
  check("a")
})
