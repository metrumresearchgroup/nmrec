prob_line <- "$prob p"

test_that("extract_theta() works", {
  cases <- list(
    list(
      lines = "$theta 1 2 3",
      want = structure(
        c(1, 2, 3),
        nmrec_record_size = 3L
      )
    ),
    list(
      lines = c(
        "$theta 1 2 3",
        "$omega 10",
        "$theta 4 5"
      ),
      want = structure(
        c(1, 2, 3, 4, 5),
        nmrec_record_size = c(3L, 2L)
      )
    ),
    list(
      lines = "$theta (1.0 1.1 1.2) 2 (3.0, 3.1)",
      want = structure(
        c(1.1, 2, 3.1),
        nmrec_record_size = 3L
      )
    ),
    list(
      lines = "$theta 1 (2.0,,2.1) 3",
      want = structure(
        c(1, NA_real_, 3),
        nmrec_record_size = 3L
      )
    ),
    list(
      lines = "$theta (1.0,1.1,1.2)x3 4",
      want = structure(
        c(1.1, NA_real_, NA_real_, 4),
        nmrec_record_size = 4L
      )
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
      want = structure(
        c(1, 2, 3),
        nmrec_record_size = 3L,
        nmrec_flags = list(
          fixed = c(FALSE, FALSE, FALSE)
        )
      )
    ),
    list(
      lines = c(
        "$theta 1 2 3 FIX",
        "$omega 10",
        "$theta 4 5"
      ),
      flags = "fix",
      want = structure(
        c(1, 2, 3, 4, 5),
        nmrec_record_size = c(3L, 2L),
        nmrec_flags = list(
          fixed = c(FALSE, FALSE, TRUE, FALSE, FALSE)
        )
      )
    ),
    list(
      lines = "$theta (1.0 1.1 1.2 fix) 2 (fix 3.0, 3.1)",
      flags = "fix",
      want = structure(
        c(1.1, 2, 3.1),
        nmrec_record_size = 3L,
        nmrec_flags = list(
          fixed = c(TRUE, FALSE, TRUE)
        )
      )
    ),
    list(
      lines = "$theta 1 FIX (2.0,,2.1 fix) 3 UNINT",
      flags = "fix",
      want = structure(
        c(1, NA_real_, 3),
        nmrec_record_size = 3L,
        nmrec_flags = list(
          fixed = c(TRUE, TRUE, FALSE)
        )
      )
    ),
    list(
      lines = "$theta (1.0,1.1,1.2 fix)x3 4 UNINT",
      flags = c("fix", "uni"),
      want = structure(
        c(1.1, NA_real_, NA_real_, 4),
        nmrec_record_size = 4L,
        nmrec_flags = list(
          fixed = c(TRUE, NA, NA, FALSE),
          unint = c(FALSE, NA, NA, TRUE)
        )
      )
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    expect_identical(
      extract_theta(ctl, mark_flags = case$flags),
      case$want
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

test_that("extract_theta(..., type = 'up') works", {
  cases <- list(
    list(
      lines = "$theta 1 2 3",
      want = structure(
        c(NA_real_, NA_real_, NA_real_),
        nmrec_record_size = 3L
      )
    ),
    list(
      lines = "$theta (1.0 1.1 1.2) 2 (3.0, 3.1)",
      want = structure(
        c(1.2, NA_real_, NA_real_),
        nmrec_record_size = 3L
      )
    ),
    list(
      lines = "$theta 1 (2.0,,2.1) 3",
      want = structure(
        c(NA_real_, 2.1, NA_real_),
        nmrec_record_size = 3L
      )
    ),
    list(
      lines = "$theta (1.0,1.1,1.2)x3 4",
      want = structure(
        c(1.2, NA_real_, NA_real_, NA_real_),
        nmrec_record_size = 4L
      )
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    expect_identical(
      extract_theta(ctl, type = "up"),
      case$want
    )
  }
})

test_that("extract_theta(..., type = 'low') works", {
  cases <- list(
    list(
      lines = "$theta 1 2 3",
      want = structure(
        c(NA_real_, NA_real_, NA_real_),
        nmrec_record_size = 3L
      )
    ),
    list(
      lines = "$theta (1.0 1.1 1.2) 2 (3.0, 3.1)",
      want = structure(
        c(1.0, NA_real_, 3.0),
        nmrec_record_size = 3L
      )
    ),
    list(
      lines = "$theta 1 (2.0,,2.1) 3",
      want = structure(
        c(NA_real_, 2.0, NA_real_),
        nmrec_record_size = 3L
      )
    ),
    list(
      lines = "$theta (1.0,1.1,1.2)x3 4",
      want = structure(
        c(1.0, NA_real_, NA_real_, NA_real_),
        nmrec_record_size = 4L
      )
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    expect_identical(
      extract_theta(ctl, type = "low"),
      case$want
    )
  }
})

test_that("extract_omega() works", {
  cases <- list(
    list(
      lines = "$omega 1 2",
      want = structure(
        matrix(
          # fmt: skip
          c(
            1, NA_real_,
            NA_real_, 2
          ),
          nrow = 2,
          ncol = 2,
          byrow = TRUE
        ),
        nmrec_record_size = 2L
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
      want = structure(
        matrix(
          # fmt: skip
          c(
            1, NA_real_, NA_real_, NA_real_,
            NA_real_, 2, NA_real_, NA_real_,
            NA_real_, NA_real_, 3, NA_real_,
            NA_real_, NA_real_, 0.1, 4
          ),
          nrow = 4,
          ncol = 4,
          byrow = TRUE
        ),
        nmrec_record_size = c(2L, 2L)
      )
    ),
    list(
      lines = "$omega block(3) VAL(1, 0.1)",
      want = structure(
        matrix(
          # fmt: skip
          c(
            1, NA_real_, NA_real_,
            0.1, NA_real_, NA_real_,
            NA_real_, NA_real_, NA_real_
          ),
          nrow = 3,
          ncol = 3,
          byrow = TRUE
        ),
        nmrec_record_size = 3L
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
      want = structure(
        matrix(
          # fmt: skip
          c(
            1, NA_real_, NA_real_, NA_real_,
            2, 3, NA_real_, NA_real_,
            NA_real_, NA_real_, NA_real_, NA_real_,
            NA_real_, NA_real_, NA_real_, NA_real_
          ),
          nrow = 4,
          ncol = 4,
          byrow = TRUE
        ),
        nmrec_record_size = c(2L, 2L)
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
      want = structure(
        matrix(
          # fmt: skip
          c(
            1, NA_real_,
            NA_real_, 2
          ),
          nrow = 2,
          ncol = 2,
          byrow = TRUE
        ),
        nmrec_record_size = 2L,
        nmrec_flags = list(
          fixed = matrix(
            # fmt: skip
            c(
              FALSE, NA,
              NA, TRUE
            ),
            nrow = 2,
            ncol = 2,
            byrow = TRUE
          )
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
      want = structure(
        matrix(
          # fmt: skip
          c(
            3, NA_real_,
            0.1, 4
          ),
          nrow = 2,
          ncol = 2,
          byrow = TRUE
        ),
        nmrec_record_size = 2L,
        nmrec_flags = list(
          fixed = matrix(
            # fmt: skip
            c(
              TRUE, NA,
              TRUE, TRUE
            ),
            nrow = 2,
            ncol = 2,
            byrow = TRUE
          )
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
      want = structure(
        matrix(
          # fmt: skip
          c(
            3, NA_real_,
            0.1, 4
          ),
          nrow = 2,
          ncol = 2,
          byrow = TRUE
        ),
        nmrec_record_size = 2L,
        nmrec_flags = list(
          fixed = matrix(
            # fmt: skip
            c(
              TRUE, NA,
              TRUE, TRUE
            ),
            nrow = 2,
            ncol = 2,
            byrow = TRUE
          ),
          unint = matrix(
            # fmt: skip
            c(
              TRUE, NA,
              TRUE, TRUE
            ),
            nrow = 2,
            ncol = 2,
            byrow = TRUE
          )
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
      want = structure(
        matrix(
          # fmt: skip
          c(
            1, NA_real_, NA_real_, NA_real_,
            NA_real_, 2, NA_real_, NA_real_,
            NA_real_, NA_real_, 3, NA_real_,
            NA_real_, NA_real_, 0.1, 4
          ),
          nrow = 4,
          ncol = 4,
          byrow = TRUE
        ),
        nmrec_record_size = c(2L, 2L),
        nmrec_flags = list(
          standard = matrix(
            # fmt: skip
            c(
              TRUE, NA, NA, NA,
              NA, FALSE, NA, NA,
              NA, NA, FALSE, NA,
              NA, NA, FALSE, FALSE
            ),
            nrow = 4,
            ncol = 4,
            byrow = TRUE
          ),
          fixed = matrix(
            # fmt: skip
            c(
              TRUE, NA, NA, NA,
              NA, FALSE, NA, NA,
              NA, NA, TRUE, NA,
              NA, NA, TRUE, TRUE
            ),
            nrow = 4,
            ncol = 4,
            byrow = TRUE
          ),
          correlation = matrix(
            # fmt: skip
            c(
              FALSE, NA, NA, NA,
              NA, FALSE, NA, NA,
              NA, NA, TRUE, NA,
              NA, NA, TRUE, TRUE
            ),
            nrow = 4,
            ncol = 4,
            byrow = TRUE
          )
        )
      )
    ),
    list(
      lines = "$omega block(3) FIX VAL(1, 0.1)",
      flags = "fix",
      want = structure(
        matrix(
          # fmt: skip
          c(
            1, NA_real_, NA_real_,
            0.1, NA_real_, NA_real_,
            NA_real_, NA_real_, NA_real_
          ),
          nrow = 3,
          ncol = 3,
          byrow = TRUE
        ),
        nmrec_record_size = 3L,
        nmrec_flags = list(
          fixed = matrix(
            # fmt: skip
            c(
              TRUE, NA, NA,
              TRUE, NA, NA,
              NA, NA, NA
            ),
            nrow = 3,
            ncol = 3,
            byrow = TRUE
          )
        )
      )
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    want <- case$want
    expect_identical(
      extract_omega(ctl, mark_flags = case$flags),
      case$want
    )
  }
})

test_that("extract_sigma() works", {
  # Note: The extract_omega() tests above covers most the shared matrix logic.
  ctl <- parse_ctl(c(prob_line, "$sigma block(2) FIX SD 1 2 3"))
  got <- extract_sigma(ctl, mark_flags = c("fixed", "SD"))
  want <- structure(
    matrix(
      # fmt: skip
      c(
        1, NA_real_,
        2, 3
      ),
      nrow = 2,
      ncol = 2,
      byrow = TRUE
    ),
    nmrec_record_size = 2L,
    nmrec_flags = list(
      fixed = matrix(
        # fmt: skip
        c(
          TRUE, NA,
          TRUE, TRUE
        ),
        nrow = 2,
        ncol = 2,
        byrow = TRUE
      ),
      standard = matrix(
        # fmt: skip
        c(
          TRUE, NA,
          TRUE, TRUE
        ),
        nrow = 2,
        ncol = 2,
        byrow = TRUE
      )
    )
  )
  expect_identical(got, want)
})

test_that("extract_omega() warns about SCALE", {
  cases <- list(
    list(
      lines = "$omega 0.5 scale(2.0) 0.8 0.9 sca(1.5) 0.1",
      want = structure(
        matrix(
          # fmt: skip
          c(
            0.5, NA_real_, NA_real_, NA_real_,
            NA_real_, 0.8, NA_real_, NA_real_,
            NA_real_, NA_real_, 0.9, NA_real_,
            NA_real_, NA_real_, NA_real_, 0.1
          ),
          nrow = 4,
          ncol = 4,
          byrow = TRUE
        ),
        nmrec_record_size = 4L
      )
    ),
    list(
      lines = c(
        "$omega scale(3) block (2)",
        "0.1",
        "0.01 0.2",
        "$omega block(2) same"
      ),
      want = structure(
        matrix(
          # fmt: skip
          c(
            0.1, NA_real_, NA_real_, NA_real_,
            0.01, 0.2, NA_real_, NA_real_,
            NA_real_, NA_real_, NA_real_, NA_real_,
            NA_real_, NA_real_, NA_real_, NA_real_
          ),
          nrow = 4,
          ncol = 4,
          byrow = TRUE
        ),
        nmrec_record_size = c(2L, 2L)
      )
    )
  )
  for (case in cases) {
    ctl <- parse_ctl(c(prob_line, case$lines))
    want <- case$want
    expect_warning(
      expect_identical(
        extract_omega(ctl),
        case$want
      ),
      "SCALE"
    )
  }
})
