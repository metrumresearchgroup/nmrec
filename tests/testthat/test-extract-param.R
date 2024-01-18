prob_line <- "$prob p"

test_that("extract_theta() works", {
  cases <- list(
    list(
      lines = "$theta 1 2 3",
      want = c(1, 2, 3)
    ),
    list(
      lines = c(
        "$theta 1 2 3",
        "$omega 10",
        "$theta 4 5"
      ),
      want = c(1, 2, 3, 4, 5)
    ),
    list(
      lines = "$theta (1.0 1.1 1.2) 2 (3.0, 3.1)",
      want = c(1.1, 2, 3.1)
    ),
    list(
      lines = "$theta 1 (2.0,,2.1) 3",
      want = c(1, NA_real_, 3)
    ),
    list(
      lines = "$theta (1.0,1.1,1.2)x3 4",
      want = c(1.1, NA_real_, NA_real_, 4)
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    expect_identical(
      extract_theta(ctl),
      case$want
    )
  }
})

test_that("extract_omega() works", {
  cases <- list(
    list(
      lines = "$omega 1 2",
      want = matrix(
        c(
          1, NA_real_,
          NA_real_, 2
        ),
        nrow = 2, ncol = 2, byrow = TRUE
      )
    ),
    list(
      lines = c(
        "$omega 1 2",
        "$theta 10 11",
        "$omega block(2)",
        "3",
        "0.1 4"
      ),
      want = matrix(
        c(
          1, NA_real_, NA_real_, NA_real_,
          NA_real_, 2, NA_real_, NA_real_,
          NA_real_, NA_real_, 3, NA_real_,
          NA_real_, NA_real_, 0.1, 4
        ),
        nrow = 4, ncol = 4, byrow = TRUE
      )
    ),
    list(
      lines = "$omega block(3) VAL(1, 0.1)",
      want = matrix(
        c(
          1, NA_real_, NA_real_,
          0.1, NA_real_, NA_real_,
          NA_real_, NA_real_, NA_real_
        ),
        nrow = 3, ncol = 3, byrow = TRUE
      )
    ),
    list(
      lines = c(
        "$OMEGA BLOCK(2)",
        "1",
        "2 3",
        "$table TIME",
        "$OMEGA BLOCK(2) SAME"
      ),
      want = matrix(
        c(
          1, NA_real_, NA_real_, NA_real_,
          2, 3, NA_real_, NA_real_,
          NA_real_, NA_real_, NA_real_, NA_real_,
          NA_real_, NA_real_, NA_real_, NA_real_
        ),
        nrow = 4, ncol = 4, byrow = TRUE
      )
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    expect_identical(
      extract_omega(ctl),
      case$want
    )
  }
})

test_that("extract_sigma() works", {
  # Note: The extract_omega() tests above covers most the shared matrix logic.
  ctl <- parse_ctl(c(prob_line, "$sigma block(2) FIX SD 1 2 3"))
  got <- extract_sigma(ctl)
  want <- matrix(
    c(
      1, NA_real_,
      2, 3
    ),
    nrow = 2, ncol = 2, byrow = TRUE
  )
  expect_identical(got, want)
})
