#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("is_function_assignment() works", {

  expect_error(
    kwb.code:::is_function_assignment()
    # argument "assignment" is missing, with no default
  )

})

