test_that("get_depth_if() works", {

  x1 <- parse(text = "if (a == 1) 1 else 2")[[1L]]
  x2 <- parse(text = "if (a == 1) {x <- 1} else {x <- 2}")[[1L]]
  
  f <- kwb.code:::get_depth_if
  
  check_names <- function(r) expect_identical(names(r), c(
    "class", "depth.cond", "depth.if", "depth.else"
  ))
  
  check_names(f(x1))
  check_names(f(x2))
})
