# Note: The underlying processing is shared with omega. See test-record-omega.R
# for more extensive testing.

test_that("parse_sigma_record() works", {
  cases <- list(
    list(
      input = "$SIGMA 1",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          sigma = option_param$new(
            "sigma",
            template = list(1L),
            values = list(init = option_pos$new("init", "1"))
          )
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_sigma$new("sigma", "SIGMA", case$input)
    res <- rec$parse()
    expect_identical(res$template, case$want$template)
    expect_identical(res$options, case$want$options)
    # Inputs and results match when rendered as string.
    expect_identical(
      format_from_template("SIGMA", res$template, res$options),
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
    expect_null(recs[[i]]$template)
    expect_null(recs[[i]]$options)
  }

  recs[[4]]$parse()

  expect_identical(
    recs[[2]]$template,
    list("record_name", elem_whitespace(" "), 1L, elem_linebreak())
  )
  expect_identical(
    recs[[2]]$options,
    list(
      sigma = option_param$new(
        "sigma",
        template = list(1L),
        values = list(init = option_pos$new("init", "1"))
      )
    )
  )

  expect_identical(
    recs[[4]]$template,
    list("record_name", elem_whitespace(" "), 1L, elem_linebreak())
  )
  expect_identical(
    recs[[4]]$options,
    list(
      sigma = option_param$new(
        "sigma",
        template = list(1L),
        values = list(init = option_pos$new("init", "2"))
      )
    )
  )

  expect_identical(
    format(res),
    paste0(paste0(lines, collapse = "\n"), "\n")
  )
})
