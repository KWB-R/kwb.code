test_that("extract_from_parse_tree() works", {

  files <- dir(
    system.file("extdata", package = "kwb.code"), 
    pattern = "testcalls", 
    full.names = TRUE
  )

  expect_equal(kwb.code:::extract_from_parse_tree(x = parse(files[1])), "f1")
  expect_equal(kwb.code:::extract_from_parse_tree(x = parse(files[2])), "f1")
})
