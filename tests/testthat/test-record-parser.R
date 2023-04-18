test_that("record_parser errors on name_raw/lines mismatch", {
  expect_error(
    record_parser$new("foo", "foo", "$foobert"),
    class = "nmrec_dev_error"
  )
})
