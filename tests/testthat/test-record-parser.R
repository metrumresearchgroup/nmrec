test_that("record_parser errors on name_raw/lines mismatch", {
  expect_error(
    record_parser$new("foo", "foo", "$foobert"),
    class = "nmrec_dev_error"
  )
})

test_that("record_parser$assert_done() fails if elems remain", {
  rp <- record_parser$new("foo", "foo", "$foo")
  expect_error(rp$assert_done(), class = "nmrec_dev_error")
})

test_that("record_parser$assert_remaining() fails if no elems remain", {
  rp <- record_parser$new("foo", "foo", "$foo")
  rp$gobble()
  expect_error(rp$assert_remaining(), class = "nmrec_dev_error")
})

test_that("record_parser$yank_to() aborts if pos upstream of current", {
  rp <- record_parser$new("foo", "foo", "$foo")
  expect_error(rp$yank_to(pos = 1), class = "nmrec_dev_error")
})

test_that("record_parser$find_next() returns 0 if no elems remain", {
  rp <- record_parser$new("foo", "foo", "$foo")
  rp$gobble()
  expect_identical(rp$find_next(), 0L)
})
