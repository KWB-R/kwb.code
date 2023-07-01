test_that("writeContentsToFiles() works", {

  f <- kwb.code:::writeContentsToFiles
  
  expect_error(f())

  target_dir <- tempdir()
  
  capture.output(f(list("a", "b"), target_dir, "f"))
  
  expect_true(all(c("f_1.txt", "f_2.txt") %in% dir(target_dir)))
})
