test_that("data_option_types and data_option_names align", {
  expect_setequal(
    names(data_option_types),
    purrr::map_chr(
      ls(data_option_names),
      ~ get(.x, envir = data_option_names)
    )
  )
})

test_that("parse_data_record() aborts if filename is not on first line", {
  rec <- record_data$new("data", "data", c("$data", "fn"))
  expect_error(rec$parse(), class = "nmrec_parse_error")
})

test_that("parse_data_record() aborts on filename=*", {
  rec <- record_data$new("data", "data", "$data *")
  expect_error(rec$parse(), class = "nmrec_unsupported")
})

test_that("parse_data_record() aborts if format option lacks closing paren", {
  rec <- record_data$new("data", "data", c("$data fn", "(FE"))
  expect_error(rec$parse(), class = "nmrec_parse_error")
})

test_that("parse_data_record() works", {
  cases <- list(
    list(
      input = "$data foo.csv",
      want = list(
        values = list(
          option_record_name$new("data", "data"),
          elem_whitespace(" "),
          option_pos$new("filename", value = "foo.csv"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$data 'foo bar.csv'",
      want = list(
        values = list(
          option_record_name$new("data", "data"),
          elem_whitespace(" "),
          option_pos$new("filename", value = "'foo bar.csv'"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = '$data "foo bar.csv"',
      want = list(
        values = list(
          option_record_name$new("data", "data"),
          elem_whitespace(" "),
          option_pos$new("filename", value = '"foo bar.csv"'),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c("$data foo.csv &  ", "(FE) rec 3"),
      want = list(
        values = list(
          option_record_name$new("data", "data"),
          elem_whitespace(" "),
          option_pos$new("filename", value = "foo.csv"),
          elem_whitespace(" "), elem_ampersand(),
          elem_whitespace("  "), elem_linebreak(),
          option_pos$new("format", value = "(FE)"),
          elem_whitespace(" "),
          option_value$new("records", "rec", value = "3", sep = " "),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$data    foo.csv ; a comment",
        "IGNORE( inside paren ) norew",
        "\t; trailing comment"
      ),
      want = list(
        values = list(
          option_record_name$new("data", "data"),
          elem_whitespace("    "),
          option_pos$new("filename", value = "foo.csv"),
          elem_whitespace(" "),
          elem_comment("; a comment"), elem_linebreak(),
          option_value$new(
            "ignore",
            name_raw = "IGNORE", value = "( inside paren )", sep = ""
          ),
          elem_whitespace(" "),
          option_flag$new("norewind", name_raw = "norew", value = TRUE),
          elem_linebreak(),
          elem_whitespace("\t"), elem_comment("; trailing comment"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$data foo.csv",
        "; comment",
        "IGNORE=C IGNORE(foo)"
      ),
      want = list(
        values = list(
          option_record_name$new("data", "data"),
          elem_whitespace(" "),
          option_pos$new("filename", value = "foo.csv"),
          elem_linebreak(), elem_comment("; comment"), elem_linebreak(),
          option_value$new(
            "ignore",
            name_raw = "IGNORE", value = "C", sep = "="
          ),
          elem_whitespace(" "),
          option_value$new(
            "ignore",
            name_raw = "IGNORE", value = "(foo)", sep = ""
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        # For paren values, the value can continue to the next line, but nmrec
        # will include comments as part of the value.
        "$data foo.csv ign (foo ; bar",
        "baz)"
      ),
      want = list(
        values = list(
          option_record_name$new("data", "data"),
          elem_whitespace(" "),
          option_pos$new("filename", value = "foo.csv"),
          elem_whitespace(" "),
          option_value$new(
            "ignore",
            name_raw = "ign", value = "(foo ; bar\nbaz)", sep = " "
          ),
          elem_linebreak()
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_data$new("data", "data", case$input)
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

test_that("data records are combined", {
  lines <- c(
    "$prob a",
    "$data foo.csv",
    "$sub bar",
    "$data\t(F )",
    "$DAT ign C"
  )

  res <- parse_ctl(lines)
  recs <- res$records

  for (i in c(2, 4, 5)) {
    expect_null(recs[[i]]$values)
  }

  recs[[5]]$parse()

  expect_identical(
    recs[[2]]$values,
    list(
      option_record_name$new("data", "data"),
      elem_whitespace(" "),
      option_pos$new("filename", value = "foo.csv"),
      elem_linebreak()
    )
  )

  expect_identical(
    recs[[4]]$values,
    list(
      option_record_name$new("data", "data"),
      elem_whitespace("\t"),
      option_pos$new("format", value = "(F )"),
      elem_linebreak()
    )
  )

  expect_identical(
    recs[[5]]$values,
    list(
      option_record_name$new("data", "DAT"),
      elem_whitespace(" "),
      option_value$new("ignore", name_raw = "ign", value = "C", sep = " "),
      elem_linebreak()
    )
  )

  expect_identical(
    format(res),
    paste0(paste0(lines, collapse = "\n"), "\n")
  )
})
