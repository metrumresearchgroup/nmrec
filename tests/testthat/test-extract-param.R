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

test_that("extract_theta(..., mark_flags = ...) works", {
  cases <- list(
    list(
      lines = "$theta 1 2 3",
      flags = "fix",
      want = c(1, 2, 3),
      want_attr = list(
        fixed = c(FALSE, FALSE, FALSE)
      )
    ),
    list(
      lines = c(
        "$theta 1 2 3 FIX",
        "$omega 10",
        "$theta 4 5"
      ),
      flags = "fix",
      want = c(1, 2, 3, 4, 5),
      want_attr = list(
        fixed = c(FALSE, FALSE, TRUE, FALSE, FALSE)
      )
    ),
    list(
      lines = "$theta (1.0 1.1 1.2 fix) 2 (fix 3.0, 3.1)",
      flags = "fix",
      want = c(1.1, 2, 3.1),
      want_attr = list(
        fixed = c(TRUE, FALSE, TRUE)
      )
    ),
    list(
      lines = "$theta 1 FIX (2.0,,2.1 fix) 3 UNINT",
      flags = "fix",
      want = c(1, NA_real_, 3),
      want_attr = list(
        fixed = c(TRUE, TRUE, FALSE)
      )
    ),
    list(
      lines = "$theta (1.0,1.1,1.2 fix)x3 4 UNINT",
      flags = c("fix", "uni"),
      want = c(1.1, NA_real_, NA_real_, 4),
      want_attr = list(
        fixed = c(TRUE, NA, NA, FALSE),
        unint = c(FALSE, NA, NA, TRUE)
      )
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    want <- case$want
    attr(want, "nmrec_flags") <- case$want_attr
    expect_identical(
      extract_theta(ctl, mark_flags = case$flags),
      want
    )
  }
})

test_that("mark_flags ignores disabled flag", {
  ctl <- parse_ctl(c(prob_line, "$THETA 1 FIX 2"))
  rec_theta <- ctl$records[[2]]
  rec_theta$parse()

  # Active
  res <- extract_theta(ctl, mark_flags = "fix")
  expect_identical(
    attr(res, "nmrec_flags"),
    list(
      fixed = c(TRUE, FALSE)
    )
  )

  # Disabled
  opt_theta <- rec_theta$get_options()[[2]]
  fix <- opt_theta$values[[3]]
  fix$value <- NULL
  res <- extract_theta(ctl, mark_flags = "fix")
  expect_identical(
    attr(res, "nmrec_flags"),
    list(
      fixed = c(FALSE, FALSE)
    )
  )
})

test_that("mark_flags errors aborts on unknown flags", {
  expect_error(
    extract_theta(NULL, mark_flags = "names"),
    "not a flag",
    class = "nmrec_error"
  )
  expect_error(
    extract_theta(NULL, mark_flags = "foobert"),
    "unknown flags",
    class = "nmrec_error"
  )
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

test_that("extract_omega(..., mark_flags = ...) works", {
  cases <- list(
    list(
      lines = "$omega 1 2 fix",
      flags = "fix",
      want = matrix(
        c(
          1, NA_real_,
          NA_real_, 2
        ),
        nrow = 2, ncol = 2, byrow = TRUE
      ),
      want_attr = list(
        fixed = matrix(
          c(
            FALSE, NA,
            NA, TRUE
          ),
          nrow = 2, ncol = 2, byrow = TRUE
        )
      )
    ),
    list(
      lines = c(
        "$omega block(2) fix",
        "3",
        "0.1 4"
      ),
      flags = "fix",
      want = matrix(
        c(
          3, NA_real_,
          0.1, 4
        ),
        nrow = 2, ncol = 2, byrow = TRUE
      ),
      want_attr = list(
        fixed = matrix(
          c(
            TRUE, NA,
            TRUE, TRUE
          ),
          nrow = 2, ncol = 2, byrow = TRUE
        )
      )
    ),
    list(
      lines = c(
        "$omega block(2)",
        "3 fix",
        "0.1 uni 4"
      ),
      flags = c("fix", "unint"),
      want = matrix(
        c(
          3, NA_real_,
          0.1, 4
        ),
        nrow = 2, ncol = 2, byrow = TRUE
      ),
      want_attr = list(
        fixed = matrix(
          c(
            TRUE, NA,
            TRUE, TRUE
          ),
          nrow = 2, ncol = 2, byrow = TRUE
        ),
        unint = matrix(
          c(
            TRUE, NA,
            TRUE, TRUE
          ),
          nrow = 2, ncol = 2, byrow = TRUE
        )
      )
    ),
    list(
      lines = c(
        "$omega 1 sd fix 2",
        "$theta 10 11",
        "$omega block(2) fix",
        "3",
        "0.1 cor 4"
      ),
      flags = c("SD", "fix", "corre"),
      want = matrix(
        c(
          1, NA_real_, NA_real_, NA_real_,
          NA_real_, 2, NA_real_, NA_real_,
          NA_real_, NA_real_, 3, NA_real_,
          NA_real_, NA_real_, 0.1, 4
        ),
        nrow = 4, ncol = 4, byrow = TRUE
      ),
      want_attr = list(
        standard = matrix(
          c(
            TRUE, NA, NA, NA,
            NA, FALSE, NA, NA,
            NA, NA, FALSE, NA,
            NA, NA, FALSE, FALSE
          ),
          nrow = 4, ncol = 4, byrow = TRUE
        ),
        fixed = matrix(
          c(
            TRUE, NA, NA, NA,
            NA, FALSE, NA, NA,
            NA, NA, TRUE, NA,
            NA, NA, TRUE, TRUE
          ),
          nrow = 4, ncol = 4, byrow = TRUE
        ),
        correlation = matrix(
          c(
            FALSE, NA, NA, NA,
            NA, FALSE, NA, NA,
            NA, NA, TRUE, NA,
            NA, NA, TRUE, TRUE
          ),
          nrow = 4, ncol = 4, byrow = TRUE
        )
      )
    ),
    list(
      lines = "$omega block(3) FIX VAL(1, 0.1)",
      flags = "fix",
      want = matrix(
        c(
          1, NA_real_, NA_real_,
          0.1, NA_real_, NA_real_,
          NA_real_, NA_real_, NA_real_
        ),
        nrow = 3, ncol = 3, byrow = TRUE
      ),
      want_attr = list(
        fixed = matrix(
          c(
            TRUE, NA, NA,
            TRUE, NA, NA,
            NA, NA, NA
          ),
          nrow = 3, ncol = 3, byrow = TRUE
        )
      )
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    want <- case$want
    attr(want, "nmrec_flags") <- case$want_attr
    expect_identical(
      extract_omega(ctl, mark_flags = case$flags),
      want
    )
  }
})

test_that("extract_sigma() works", {
  # Note: The extract_omega() tests above covers most the shared matrix logic.
  ctl <- parse_ctl(c(prob_line, "$sigma block(2) FIX SD 1 2 3"))
  got <- extract_sigma(ctl, mark_flags = c("fixed", "SD"))
  want <- matrix(
    c(
      1, NA_real_,
      2, 3
    ),
    nrow = 2, ncol = 2, byrow = TRUE
  )
  attr(want, "nmrec_flags") <- list(
    fixed = matrix(
      c(
        TRUE, NA,
        TRUE, TRUE
      ),
      nrow = 2, ncol = 2, byrow = TRUE
    ),
    standard = matrix(
      c(
        TRUE, NA,
        TRUE, TRUE
      ),
      nrow = 2, ncol = 2, byrow = TRUE
    )
  )
  expect_identical(got, want)
})
