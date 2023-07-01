#library(testthat)

f1 <- function() { x <- 1 }

test_that("duplicatesToFiles() works", {

  f <- kwb.code:::duplicatesToFiles
  
  expect_error(f())

  root <- if ("tests" %in% dir()) "./tests/testthat/" else "."
  
  #writeLines("f1 <- function(x) x + 1", file.path(root, "script.R"))
  
  capture.output(trees <- kwb.code::parse_scripts(root = root))

  fun_duplicates <- data.frame(
    script = "test-function-duplicatesToFiles.R",
    functionName = "f1"
  )
  
  capture.output(path <- f(trees, fun_duplicates, function_name = "f1"))
  
  expect_true(file.exists(path))
  expect_true(length(dir(path, "^f1")) > 0L)

})
