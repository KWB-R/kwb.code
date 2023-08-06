test_that("writeContentsToFiles() works", {

  f <- kwb.code:::writeContentsToFiles
  
  expect_error(f())

  target_dir <- tempdir()
  
  capture.output(f(list("a", "b"), target_dir, "f"))
  
  expect_true(all(c("f__1.txt", "f__2.txt") %in% dir(target_dir)))
})
