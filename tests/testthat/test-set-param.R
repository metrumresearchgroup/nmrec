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
  ctl <- parse_ctl(c(prob_line, "$sigma block(2) 1 2 3"))
  set_sigma(ctl, 4:6)
  expect_identical(
    format(ctl),
    "$prob p\n$sigma block(2) 4 5 6\n"
  )
})
