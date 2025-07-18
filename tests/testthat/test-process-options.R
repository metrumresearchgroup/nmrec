test_that("process_options aborts on unknown value", {
  expect_error(
    record_estimation$new("estimation", "est", "$est methood=1")$parse(),
    regexp = "unknown option",
    ignore.case = TRUE,
    class = "nmrec_parse_error"
  )
})

test_that("process_options aborts on missing value", {
  cases <- c(
    "$table num varcalc",
    "$table num varcalc=",
    "$table num varcalc ; foo",
    "$table num format"
  )
  for (case in cases) {
    expect_error(
      record_table$new("table", "table", !!case)$parse(),
      regexp = "missing value",
      ignore.case = TRUE,
      class = "nmrec_parse_error"
    )
  }
})

test_that("process_options aborts on incomplete format specification after comma", {
  cases <- c(
    "$table num format=,",
    "$table num format=, ",
    "$table num format=,;comment"
  )
  for (case in cases) {
    expect_error(
      record_table$new("table", "table", !!case)$parse(),
      regexp = "incomplete specification",
      ignore.case = TRUE,
      class = "nmrec_parse_error"
    )
  }
})
