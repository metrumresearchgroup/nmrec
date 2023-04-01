test_that("prior_option_types and prior_option_names align", {
  expect_setequal(
    names(prior_option_types),
    purrr::map_chr(
      ls(prior_option_names),
      ~ get(.x, envir = prior_option_names)
    )
  )
})

test_that("parse_prior_record() works", {
  cases <- list(
    list(
      input = c(
        "$PRIOR TNPRI (PROBLEM 2) PLEV=.9999",
        "iss=0 IVAR=1"
      ),
      want = list(
        values = list(
          elem_whitespace(" "),
          option_flag$new("tnpri", name_raw = "TNPRI", value = TRUE),
          elem_whitespace(" "),
          option_pos$new("clause", value = "(PROBLEM 2)"),
          elem_whitespace(" "),
          option_value$new("plev", name_raw = "PLEV", value = ".9999"),
          elem_linebreak(),
          option_value$new("iss", name_raw = "iss", value = "0"),
          elem_whitespace(" "),
          option_value$new("ivar", name_raw = "IVAR", value = "1"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$PRIOR TNPRI (EST, PROB 3) IFND=1",
        "       TNPRI (EST, PROB 4) IFND=2"
      ),
      want = list(
        values = list(
          elem_whitespace(" "),
          option_flag$new("tnpri", name_raw = "TNPRI", value = TRUE),
          elem_whitespace(" "),
          option_pos$new("clause", value = "(EST, PROB 3)"),
          elem_whitespace(" "),
          option_value$new("ifnd", name_raw = "IFND", value = "1"),
          elem_linebreak(),
          elem_whitespace("       "),
          option_flag$new("tnpri", name_raw = "TNPRI", value = TRUE),
          elem_whitespace(" "),
          option_pos$new("clause", value = "(EST, PROB 4)"),
          elem_whitespace(" "),
          option_value$new("ifnd", name_raw = "IFND", value = "2"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$PRIOR nwpri ntheta=1 neta=2 nthp=3 netp=4 npexp=5",
      want = list(
        values = list(
          elem_whitespace(" "),
          option_flag$new("nwpri", name_raw = "nwpri", value = TRUE),
          elem_whitespace(" "),
          option_value$new("ntheta", name_raw = "ntheta", value = "1"),
          elem_whitespace(" "),
          option_value$new("neta", name_raw = "neta", value = "2"),
          elem_whitespace(" "),
          option_value$new("nthp", name_raw = "nthp", value = "3"),
          elem_whitespace(" "),
          option_value$new("netp", name_raw = "netp", value = "4"),
          elem_whitespace(" "),
          option_value$new("npexp", name_raw = "npexp", value = "5"),
          elem_linebreak()
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_prior$new("prior", "PRIOR", case$input)
    rec$parse()
    expect_false(is.null(rec$values))
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

test_that("prior records are combined", {
  lines <- c(
    "$prob",
    "$PRIOR nwpri ntheta=1",
    "$data foo.csv",
    "$prior neta=2"
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
      option_flag$new("nwpri", name_raw = "nwpri", value = TRUE),
      elem_whitespace(" "),
      option_value$new("ntheta", name_raw = "ntheta", value = "1"),
      elem_linebreak()
    )
  )

  expect_identical(
    recs[[4]]$values,
    list(
      elem_whitespace(" "),
      option_value$new("neta", name_raw = "neta", value = "2"),
      elem_linebreak()
    )
  )

  expect_identical(
    format(res),
    paste0(paste0(lines, collapse = "\n"), "\n")
  )
})
