test_that("set_record_option() can modify, remove, or add an option_flag option", {
  ctl <- parse_ctl(get("bayes1", envir = nmrec_examples))

  recs <- select_records(ctl, "est")
  rec <- recs[[1]]

  # Remove
  want <- option_flag$new("noabort", name_raw = "NOABORT", value = NULL)
  set_record_option(rec, "NOABORT", value = NULL)

  expect_identical(
    get_record_option(rec, "NOABORT"),
    want
  )

  # Add back (modify)
  want <- option_flag$new("noabort", name_raw = "NOABORT", value = TRUE)
  set_record_option(rec, "NOABORT", value = TRUE)

  expect_identical(
    get_record_option(rec, "NOABORT"),
    want
  )

  # Add new flag
  want <- option_flag$new("posthoc", name_raw = "POSTHOC", value = TRUE)
  set_record_option(rec, name = "POSTHOC", value = TRUE)

  expect_identical(
    get_record_option(rec, "POSTHOC"),
    want
  )
})

test_that("set_record_option() can modify, remove, or add an option_value option", {
  ctl <- parse_ctl(get("bayes1", envir = nmrec_examples))
  recs <- select_records(ctl, "est")
  expect_length(recs, 5)
  rec <- recs[[1]]

  ## numeric value ##

  # Modify
  want <- option_value$new("citer", name_raw = "CITER", value = 15)
  set_record_option(rec, "CITER", value = 15)

  expect_identical(
    get_record_option(rec, "CITER"),
    want
  )

  # Remove
  want <- option_value$new("citer", name_raw = "CITER", value = NULL)
  set_record_option(rec, "CITER", value = NULL)

  expect_identical(
    get_record_option(rec, "CITER"),
    want
  )

  # Add Back
  want <- option_value$new("citer", name_raw = "CITER", value = 10)
  set_record_option(rec, "CITER", value = 10)

  expect_identical(
    get_record_option(rec, "CITER"),
    want
  )

  # Add New value
  want <- option_value$new("maxevals", name_raw = "MAX", value = 10)
  set_record_option(rec, "MAX", value = 10)

  expect_identical(
    get_record_option(rec, "MAX"),
    want
  )

  ## character value ##

  # Modify
  want <- option_value$new("file", name_raw = "FILE", value = "example2.ext")
  set_record_option(rec, "FILE", value = "example2.ext")

  expect_identical(
    get_record_option(rec, "FILE"),
    want
  )

  # Remove
  want <- option_value$new("file", name_raw = "FILE", value = NULL)
  set_record_option(rec, "FILE", value = NULL)

  expect_identical(
    get_record_option(rec, "FILE"),
    want
  )

  # Add
  rec <- recs[[2]]
  want <- option_value$new("file", name_raw = "FILE", value = "example1.ext")
  set_record_option(rec, "FILE", value = "example1.ext")

  expect_identical(
    get_record_option(rec, "FILE"),
    want
  )
})

test_that("set_record_option() errors for invalid options", {
  ctl <- parse_ctl(get("bayes1", envir = nmrec_examples))
  recs <- select_records(ctl, "est")
  expect_error(
    set_record_option(recs[[1]], "MAXF", value = 4),
    class = "nmrec_error"
  )
})

test_that("set_record_option() works with special characters and formats correctly", {
  # `nburn = 15` will be appended to each case
  cases <- list(
    list(
      input = c(
        "$est max=99",
        ""
      ),
      want = list(
        option_record_name$new("estimation", "est"),
        elem_whitespace(" "),
        option_value$new("maxevals", name_raw = "max", value = "99", sep = "="),
        elem_whitespace(" "),
        option_value$new("nburn", name_raw = "nburn", value = "15", sep = "="),
        elem_linebreak(),
        elem_linebreak()
      )
    ),
    list(
      input = c(
        "$est max=99 citer=10;"
      ),
      want = list(
        option_record_name$new("estimation", "est"),
        elem_whitespace(" "),
        option_value$new("maxevals", name_raw = "max", value = "99", sep = "="),
        elem_whitespace(" "),
        option_value$new("citer", name_raw = "citer", value = "10", sep = "="),
        elem_whitespace(" "),
        option_value$new("nburn", name_raw = "nburn", value = "15", sep = "="),
        elem_comment(";"),
        elem_linebreak()
      )
    ),
    list(
      input = c(
        "$est max=99 &",
        "citer=10"
      ),
      want = list(
        option_record_name$new("estimation", "est"),
        elem_whitespace(" "),
        option_value$new("maxevals", name_raw = "max", value = "99", sep = "="),
        elem_whitespace(" "),
        elem_ampersand(),
        elem_linebreak(),
        option_value$new("citer", name_raw = "citer", value = "10", sep = "="),
        elem_whitespace(" "),
        option_value$new("nburn", name_raw = "nburn", value = "15", sep = "="),
        elem_linebreak()
      )
    ),
    list(
      input = c(
        "$est max=99&"
      ),
      want = list(
        option_record_name$new("estimation", "est"),
        elem_whitespace(" "),
        option_value$new("maxevals", name_raw = "max", value = "99", sep = "="),
        elem_whitespace(" "),
        option_value$new("nburn", name_raw = "nburn", value = "15", sep = "="),
        elem_ampersand(),
        elem_linebreak()
      )
    )
  )

  for (case in cases) {
    rec <- record_estimation$new("estimation", "est", case$input)

    set_record_option(rec, "nburn", value = "15")

    expect_identical(rec$values, case$want)
    # Inputs and results match when rendered as string.
    want_lines <- unlist(strsplit(lstr_format(case$want), "\n"))
    rec_want <- record_estimation$new("estimation", "est", want_lines)
    rec_want$parse()

    expect_identical(
      rec$format(),
      lstr_format(case$want)
    )
    expect_identical(
      rec,
      rec_want
    )
  }
})
