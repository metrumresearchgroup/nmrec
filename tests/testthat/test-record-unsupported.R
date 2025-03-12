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

test_that("theta: unint cannot be abbreviated beyond 'uni'", {
  cases <- list(
    "$THETA 1 u",
    "$THETA 1 un",
    # nmrec behavior is actually consistent with NM-TRAN here; it doesn't permit
    # u and un within parens.
    "$THETA (1 2 u)",
    "$THETA (1 2 un)"
  )

  for (case in cases) {
    expect_error(
      record_theta$new("theta", "THETA", case)$parse(),
      class = "nmrec_unsupported"
    )
  }
})

test_that("parse_omega_record() aborts if scale between init options", {
  cases <- list(
    "$OMEGA SCALE(0.1) 0.1 SCALE(2) sd 0.3",
    "$OMEGA SCALE(0.1) 0.1 SCALE(2) FIX 0.3",
    "$OMEGA SCALE(0.1) 0.1 sd SCALE(2) fix 0.3"
  )

  for (case in cases) {
    rec <- record_omega$new("omega", "OMEGA", case)
    expect_error(rec$parse(), class = "nmrec_parse_error")
  }
})
