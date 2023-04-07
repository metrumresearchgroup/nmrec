test_that("record$name is read-only", {
  rec <- record$new("foo", "fo", "$fo bar")
  expect_identical(rec$name, "foo")
  expect_error(rec$name <- "x", class = "nmrec_error")
})
