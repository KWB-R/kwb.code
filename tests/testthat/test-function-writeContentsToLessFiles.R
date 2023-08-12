test_that("writeContentsToLessFiles() works", {

  f <- kwb.code:::writeContentsToLessFiles
  
  expect_error(f())

  target_dir <- tempdir()
  
  capture.output(f(list("a", "b"), target_dir, "f"))
  
  expect_true(all(c("f__unique-1.txt", "f__unique-2.txt") %in% dir(target_dir)))
})
