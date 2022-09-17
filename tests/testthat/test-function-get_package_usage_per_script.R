test_that("get_package_usage_per_script() works", {

  f <- kwb.code:::get_package_usage_per_script
  
  expect_error(f())

  result <- f("./R", packages = "kwb.utils")
  
  expect_s3_class(result, "data.frame")
  
  expect_true(all(c("package", "script", "name") %in% names(result)))
})
