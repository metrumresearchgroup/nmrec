# Note: The underlying processing is shared with omega. See test-record-omega.R
# for more extensive testing.

test_that("parse_sigma_record() works", {
  cases <- list(
    list(
      input = "$SIGMA 1",
      want = list(
        values = list(
          elem_whitespace(" "),
          option_param$new(
            "sigma",
            values = list(option_pos$new("init", "1"))
          ),
          elem_linebreak()
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_sigma$new("sigma", "SIGMA", case$input)
    rec$parse()
    expect_identical(rec$values, case$want$values)
    # Inputs and results match when rendered as string.
    expect_identical(
      rec$format(),
      paste0(
        paste0(case$input, collapse = "\n"),
        "\n"
      )
    )
  }
})

test_that("sigma records are combined", {
  lines <- c(
    "$prob",
    "$sigma 1",
    "$data foo.csv",
    "$sigma 2"
  )

  res <- parse_ctl(lines)
  recs <- res$records

  for (i in c(2, 4)) {
    expect_null(recs[[i]]$values)
  }

  recs[[4]]$parse()

  expect_identical(
    recs[[2]]$values,
    list(
      elem_whitespace(" "),
      option_param$new(
        "sigma",
        values = list(option_pos$new("init", "1"))
      ),
      elem_linebreak()
    )
  )

  expect_identical(
    recs[[4]]$values,
    list(
      elem_whitespace(" "),
      option_param$new(
        "sigma",
        values = list(option_pos$new("init", "2"))
      ),
      elem_linebreak()
    )
  )

  expect_identical(
    format(res),
    paste0(paste0(lines, collapse = "\n"), "\n")
  )
})
