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
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "),
          2L, elem_whitespace(" "),
          3L, elem_linebreak(),
          4L, elem_whitespace(" "),
          5L, elem_linebreak()
        ),
        options = list(
          tnpri = option_flag$new("tnpri", name_raw = "TNPRI", value = TRUE),
          clause = option_pos$new("clause", value = "(PROBLEM 2)"),
          plev = option_value$new("plev", name_raw = "PLEV", value = ".9999"),
          iss = option_value$new("iss", name_raw = "iss", value = "0"),
          ivar = option_value$new("ivar", name_raw = "IVAR", value = "1")
        )
      )
    ),
    list(
      input = c(
        "$PRIOR TNPRI (EST, PROB 3) IFND=1",
        "       TNPRI (EST, PROB 4) IFND=2"
      ),
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "),
          2L, elem_whitespace(" "),
          3L, elem_linebreak(),
          elem_whitespace("       "),
          4L, elem_whitespace(" "),
          5L, elem_whitespace(" "),
          6L, elem_linebreak()
        ),
        options = list(
          tnpri = option_flag$new("tnpri", name_raw = "TNPRI", value = TRUE),
          clause = option_pos$new("clause", value = "(EST, PROB 3)"),
          ifnd = option_value$new("ifnd", name_raw = "IFND", value = "1"),
          tnpri = option_flag$new("tnpri", name_raw = "TNPRI", value = TRUE),
          clause = option_pos$new("clause", value = "(EST, PROB 4)"),
          ifnd = option_value$new("ifnd", name_raw = "IFND", value = "2")
        )
      )
    ),
    list(
      input = "$PRIOR nwpri ntheta=1 neta=2 nthp=3 netp=4 npexp=5",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "),
          2L, elem_whitespace(" "),
          3L, elem_whitespace(" "),
          4L, elem_whitespace(" "),
          5L, elem_whitespace(" "),
          6L, elem_linebreak()
        ),
        options = list(
          nwpri = option_flag$new("nwpri", name_raw = "nwpri", value = TRUE),
          ntheta = option_value$new("ntheta", name_raw = "ntheta", value = "1"),
          neta = option_value$new("neta", name_raw = "neta", value = "2"),
          nthp = option_value$new("nthp", name_raw = "nthp", value = "3"),
          netp = option_value$new("netp", name_raw = "netp", value = "4"),
          npexp = option_value$new("npexp", name_raw = "npexp", value = "5")
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_prior$new("prior", "PRIOR", case$input)
    rec$parse()
    expect_identical(rec$template, case$want$template)
    expect_identical(rec$options, case$want$options)
    # Inputs and results match when rendered as string.
    expect_identical(
      format_from_template("PRIOR", rec$template, rec$options),
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
    expect_null(recs[[i]]$template)
    expect_null(recs[[i]]$options)
  }

  recs[[4]]$parse()

  expect_identical(
    recs[[2]]$template,
    list(
      "record_name", elem_whitespace(" "),
      1L, elem_whitespace(" "),
      2L, elem_linebreak()
    )
  )
  expect_identical(
    recs[[2]]$options,
    list(
      nwpri = option_flag$new("nwpri", name_raw = "nwpri", value = TRUE),
      ntheta = option_value$new("ntheta", name_raw = "ntheta", value = "1")
    )
  )

  expect_identical(
    recs[[4]]$template,
    list("record_name", elem_whitespace(" "), 1L, elem_linebreak())
  )
  expect_identical(
    recs[[4]]$options,
    list(neta = option_value$new("neta", name_raw = "neta", value = "2"))
  )

  expect_identical(
    format(res),
    paste0(paste0(lines, collapse = "\n"), "\n")
  )
})
