# Test record syntax that NM-TRAN accepts but nmrec doesn't.

test_that("option values cannot be put on next line", {
  cases <- list(
    c(
      "$est max",
      "99"
    ),
    c(
      "$est max",
      "(99)"
    ),
    c(
      "$est max=",
      "99"
    )
  )

  for (case in cases) {
    expect_error(
      record_estimation$new("estimation", "est", case)$parse(),
      class = "nmrec_parse_error"
    )
  }
})
