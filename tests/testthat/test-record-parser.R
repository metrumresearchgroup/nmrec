test_that("record_parser errors on name_raw/lines mismatch", {
  expect_error(
    record_parser$new("foo", "foo", "$foobert"),
    class = "nmrec_dev_error"
  )
})

test_that("record_parser$find_next() returns 0 if no elems remain", {
  rp <- record_parser$new("foo", "foo", "$foo")
  rp$gobble()
  expect_identical(rp$find_next(), 0L)
})
