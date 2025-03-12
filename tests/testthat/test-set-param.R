prob_line <- "$prob p"

test_that("set_theta() works", {
  cases <- list(
    list(
      lines = "$theta 1 2 3",
      values = 4:6,
      want = "$theta 4 5 6"
    ),
    list(
      lines = c(
        "$theta 1 2   3",
        "$table time",
        "$theta 4 ; c1",
        "       5 ; c2"
      ),
      values = 11:15,
      want = c(
        "$theta 11 12   13",
        "$table time",
        "$theta 14 ; c1",
        "       15 ; c2"
      )
    ),
    list(
      lines = "$theta 1 2 3",
      values = c(4, NA, 6),
      want = "$theta 4 2 6"
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    set_theta(ctl, case$values)
    expect_identical(
      format(ctl),
      paste0(paste(c(prob_line, case$want), collapse = "\n"), "\n")
    )
  }
})

test_that("set_{theta,omega,sigma}() support specifying value format", {
  ctl <- parse_ctl(c(
    prob_line,
    "$theta 1 2 3",
    "$omega 4 5 6",
    "$sigma 7 8 9"
  ))

  theta <- 1230:1232 / 1e12
  omega <- c(
    1330,
    NA, 1331,
    NA, NA, 1332
  )
  omega <- omega / 1e12
  sigma <- c(
    1430,
    NA, 1431,
    NA, NA, 1432
  )
  sigma <- sigma / 1e12

  set_theta(ctl, theta)
  set_omega(ctl, omega)
  set_sigma(ctl, sigma)

  expect_identical(
    format(ctl),
    paste(
      c(
        prob_line,
        "$theta 1.23E-09 1.23E-09 1.23E-09",
        "$omega 1.33E-09 1.33E-09 1.33E-09",
        "$sigma 1.43E-09 1.43E-09 1.43E-09\n"
      ),
      collapse = "\n"
    )
  )

  set_theta(ctl, theta, fmt = "%.5G")
  set_omega(ctl, omega, fmt = "%.5G")
  set_sigma(ctl, sigma, fmt = "%.5G")

  expect_identical(
    format(ctl),
    paste(
      c(
        prob_line,
        "$theta 1.23E-09 1.231E-09 1.232E-09",
        "$omega 1.33E-09 1.331E-09 1.332E-09",
        "$sigma 1.43E-09 1.431E-09 1.432E-09\n"
      ),
      collapse = "\n"
    )
  )
})

test_that("set_theta() aborts on size mismatch", {
  ctl <- parse_ctl(c(prob_line, "$theta 1 2 3"))
  expect_error(set_theta(ctl, 1), "Expected length")
  expect_error(set_theta(ctl, 1:4), "Expected length")
})

test_that("set_theta() works: keep bounds", {
  cases <- list(
    list(
      lines = "$theta (1,10,20) 2 3",
      values = 4:6,
      want = "$theta (1,4,20) 5 6"
    ),
    list(
      lines = "$theta (1,10) 2 3",
      values = 4:6,
      want = "$theta (1,4) 5 6"
    ),
    list(
      lines = "$theta (1,,10) 2 3",
      values = 4:6,
      want = "$theta (1,4,10) 5 6"
    ),
    list(
      lines = "$theta (1,10 FIX) 2 3",
      values = 4:6,
      want = "$theta (1,4 FIX) 5 6"
    ),
    list(
      lines = "$theta (1, ,20) 2 3",
      values = 4:6,
      want = "$theta (1, 4,20) 5 6"
    ),
    list(
      lines = "$theta (1, 10, 20) 2 3",
      values = 4:6,
      want = "$theta (1, 4, 20) 5 6"
    ),
    list(
      lines = "$theta (1, 10 FIX) 2 3",
      values = 4:6,
      want = "$theta (1, 4 FIX) 5 6"
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    set_theta(ctl, case$values)
    expect_identical(
      format(ctl),
      paste0(paste(c(prob_line, case$want), collapse = "\n"), "\n")
    )
  }
})

test_that("set_theta() works: discard bounds", {
  cases <- list(
    list(
      lines = "$theta (1,10,20) 2 3",
      values = 4:6,
      want = "$theta 4 5 6"
    ),
    list(
      lines = "$theta (1,10) 2 3",
      values = 4:6,
      want = "$theta 4 5 6"
    ),
    list(
      lines = "$theta (1,,10) 2 3",
      values = 4:6,
      want = "$theta 4 5 6"
    ),
    list(
      lines = "$theta (1,10 FIX) 2 3",
      values = 4:6,
      want = "$theta (4 FIX) 5 6"
    ),
    list(
      lines = "$theta (1, ,20) 2 3",
      values = 4:6,
      want = "$theta 4 5 6"
    ),
    list(
      lines = "$theta (1, 10, 20) 2 3",
      values = 4:6,
      want = "$theta 4 5 6"
    ),
    list(
      lines = "$theta (1, 10 FIX) 2 3",
      values = 4:6,
      want = "$theta (4 FIX) 5 6"
    ),
    list(
      lines = c(
        "$theta (1, 2, 3)",
        "$table time",
        "$theta (4, 5, 6) (7, 8, 9)"
      ),
      values = c(20, NA, 80),
      want = c(
        "$theta 20",
        "$table time",
        "$theta (4, 5, 6) 80"
      )
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    set_theta(ctl, case$values, bounds = "discard")
    expect_identical(
      format(ctl),
      paste0(paste(c(prob_line, case$want), collapse = "\n"), "\n")
    )
  }
})

test_that("set_omega() works", {
  cases <- list(
    list(
      lines = "$omega 1 2 3",
      values = c(
        4,
        0, 5,
        0, 0, 6
      ),
      want = "$omega 4 5 6"
    ),
    list(
      lines = "$omega 1 2 3 SD",
      values = matrix(
        c(
          4, 0, 0,
          0, 5, 0,
          0, 0, 6
        ),
        nrow = 3, byrow = TRUE,
      ),
      want = "$omega 4 5 6 SD"
    ),
    list(
      lines = c(
        "$omega BLOCK(2)",
        "1.1 ; c1",
        "1.2  1.3",
        "$table time",
        "$omega BLOCK(3) VALUES(1.4, 1.5) ; diag, odiag"
      ),
      values = matrix(
        c(
          2, 0, 0, 0, 0,
          3, 4, 0, 0, 0,
          0, 0, 5, 0, 0,
          0, 0, 6, 0, 0,
          0, 0, 0, 0, 0
        ),
        nrow = 5, byrow = TRUE,
      ),
      want = c(
        "$omega BLOCK(2)",
        "2 ; c1",
        "3  4",
        "$table time",
        "$omega BLOCK(3) VALUES(5, 6) ; diag, odiag"
      )
    ),
    list(
      lines = c(
        "$omega 1.1",
        "$omega BLOCK(3) VALUES(1.2, 1.3)"
      ),
      values = matrix(
        c(
          2, 0, 0, 0,
          0, 3, 0, 0,
          0, NA, 0, 0,
          0, 0, 0, 0
        ),
        nrow = 4, byrow = TRUE,
      ),
      want = c(
        "$omega 2",
        "$omega BLOCK(3) VALUES(3, 1.3)"
      )
    ),
    # Note: There's no warning about SCALE here because the second record isn't
    # touched by set_omega.
    list(
      lines = c(
        "$omega 0.5 0.8 0.9",
        "$omega sca(1.5) 0.1"
      ),
      values = c(
        1,
        0, NA,
        0, 0, NA,
        0, 0, 0, NA
      ),
      want = "$omega 1 0.8 0.9\n$omega sca(1.5) 0.1"
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    set_omega(ctl, case$values)
    expect_identical(
      format(ctl),
      paste0(paste(c(prob_line, case$want), collapse = "\n"), "\n")
    )
  }
})

test_that("set_omega() resets to variance/covariance", {
  cases <- list(
    list(
      lines = "$omega (1 SD) 2 3",
      values = c(
        4,
        0, 5,
        0, 0, 6
      ),
      want = "$omega 4 5 6"
    ),
    list(
      lines = "$omega 1 2 3 SD",
      values = c(
        4,
        0, 5,
        0, 0, 6
      ),
      want = "$omega 4 5 6"
    ),
    list(
      lines = "$omega 1 2 SD 3 SD",
      values = c(
        4,
        0, NA,
        0, 0, 6
      ),
      want = "$omega 4 2 SD 6"
    ),
    list(
      lines = c(
        "$omega 1.1 SD 1.2",
        "$table time",
        "$omega BLOCK(2) CHO",
        "2.1",
        "2.2 3.3"
      ),
      values = c(
        3,
        0, 4,
        0, 0, 5,
        0, 0, 6, 7
      ),
      want = c(
        "$omega 3 4",
        "$table time",
        "$omega BLOCK(2)",
        "5",
        "6 7"
      )
    ),
    list(
      lines = c(
        "$omega 1.1 SD 1.2 SD",
        "$table time",
        "$omega BLOCK(2) SD CORR",
        "2.1",
        "2.2 2.3",
        "$omega 3.3"
      ),
      values = c(
        NA,
        0, 4,
        0, 0, 5,
        0, 0, 6, NA,
        0, 0, 0, 0, NA
      ),
      want = c(
        "$omega 1.1 SD 4",
        "$table time",
        "$omega BLOCK(2)",
        "5",
        "6 2.3",
        "$omega 3.3"
      )
    ),
    list(
      lines = c(
        "$omega BLOCK(2), SD,CORR ; c",
        "2.1",
        "2.2 2.3"
      ),
      values = c(
        4,
        5, 6
      ),
      want = c(
        "$omega BLOCK(2) ; c",
        "4",
        "5 6"
      )
    ),
    list(
      lines = "$omega BLOCK(3) SD VALUES(1,2)",
      values = c(3, 4, 0, 0, 0, 0),
      want = "$omega BLOCK(3) VALUES(3,4)"
    ),
    list(
      lines = "$omega BLOCK(3) VALUES(1,2) SD",
      values = c(3, 4, 0, 0, 0, 0),
      want = "$omega BLOCK(3) VALUES(3,4)"
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    set_omega(ctl, case$values, representation = "reset")
    expect_identical(
      format(ctl),
      paste0(paste(c(prob_line, case$want), collapse = "\n"), "\n")
    )
  }
})

test_that("set_omega() warns if SCALE is present", {
  cases <- list(
    list(
      lines = "$omega 0.5 scale(2.0) 0.8 0.9 sca(1.5) 0.1",
      values = c(
        1,
        0, NA,
        0, 0, 3,
        0, 0, 0, 4
      ),
      want = "$omega 1 scale(2.0) 0.8 3 sca(1.5) 4"
    ),
    list(
      lines = c(
        "$omega 0.5 0.8 0.9",
        "$omega sca(1.5) 0.1"
      ),
      values = c(
        NA,
        0, NA,
        0, 0, NA,
        0, 0, 0, 2
      ),
      want = "$omega 0.5 0.8 0.9\n$omega sca(1.5) 2"
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    expect_warning(
      set_omega(ctl, case$values, representation = "reset"),
      "SCALE"
    )
    expect_identical(
      format(ctl),
      paste0(paste(c(prob_line, case$want), collapse = "\n"), "\n")
    )
  }
})

test_that("set_omega() aborts: invalid values", {
  ctl <- parse_ctl(c(prob_line, "$omega block(2) 1 2 3"))
  expect_error(
    set_omega(ctl, array(1)),
    "must have 0 or 2 dimensions"
  )
  expect_error(
    set_omega(ctl, matrix(1:6, nrow = 3)),
    "square"
  )
})

test_that("set_{theta,omega}() abort: all NAs", {
  ctl <- parse_ctl(
    c(
      prob_line,
      "$theta 1 2",
      "$omega 3 4"
    )
  )
  expect_error(
    set_theta(ctl, c(NA, NA)),
    "is NA"
  )
  expect_error(
    set_omega(ctl, c(NA, NA, NA)),
    "is NA"
  )
})

test_that("set_sigma() works", {
  # Note: The set_omega() tests above covers most the shared matrix logic.
  ctl <- parse_ctl(c(prob_line, "$sigma block(2) SD 1 2 3"))
  set_sigma(ctl, 4:6, representation = "reset")
  expect_identical(
    format(ctl),
    "$prob p\n$sigma block(2) 4 5 6\n"
  )
})
