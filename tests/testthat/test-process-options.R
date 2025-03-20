test_that("process_options aborts on unknown value", {
  expect_error(
    record_estimation$new("estimation", "est", "$est methood=1")$parse(),
    regexp = "unknown option", ignore.case = TRUE,
    class = "nmrec_parse_error"
  )
})
