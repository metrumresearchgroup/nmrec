test_that("option$name is read-only", {
  opt <- option$new("foo")
  expect_identical(opt$name, "foo")
  expect_error(opt$name <- "x", class = "nmrec_error")
})
