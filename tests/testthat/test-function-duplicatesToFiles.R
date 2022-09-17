#library(testthat)

test_that("duplicatesToFiles() works", {

  f <- kwb.code:::duplicatesToFiles
  
  expect_error(f())

  trees <- kwb.code::parse_scripts("./R")

  fun_duplicates <- data.frame(
    script = "main.R",
    functionName = "parse_scripts"
  )
  
  path <- f(trees, fun_duplicates, function_name = "parse_scripts")
  
  expect_true(file.exists(path))
  expect_true(length(dir(path, "^parse_scripts")) > 0L)

})
