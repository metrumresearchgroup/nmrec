test_that("record$name is read-only", {
  rec <- record$new("foo", "fo", "$fo bar")
  expect_identical(rec$name, "foo")
  expect_error(rec$name <- "x", class = "nmrec_error")
})

test_that("record$get_options() triggers parse if needed", {
  rec <- record_data$new("data", "data", "$data foo.csv")
  expect_null(rec$values)
  expect_identical(
    rec$get_options(),
    list(
      option_record_name$new("data", "data"),
      option_pos$new("filename", value = "foo.csv")
    )
  )
})

test_that("record$get_options() aborts on unsupported record type", {
  rec <- record$new("foo", "foo", "$foo")
  expect_error(rec$get_options(), class = "nmrec_unsupported")
})
