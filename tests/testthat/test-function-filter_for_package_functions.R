test_that("filter_for_package_functions() works", {

  # Frequency data
  fd <- data.frame(
    name = c("is_what", "kwb.code::is_what", "kwb.code:::is_what", "x"),
    count = c(1L, 2L, 3L, 4L),
    script = "script"
  )
  
  result <- kwb.code:::filter_for_package_functions(fd, "kwb.code")
  
  expect_true(nrow(result) == 1L)
  expect_true(all(c("explicit", "implicit", "count") %in% names(result)))
  expect_identical(result$explicit, 5L)
  expect_identical(result$implicit, 1L)
  expect_identical(result$count, 6L)
})
