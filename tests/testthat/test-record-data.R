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
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          filename = option_pos$new("filename", value = "foo.csv")
        )
      )
    ),
    list(
      input = "$data 'foo bar.csv'",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          filename = option_pos$new("filename", value = "'foo bar.csv'")
        )
      )
    ),
    list(
      input = '$data "foo bar.csv"',
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          filename = option_pos$new("filename", value = '"foo bar.csv"')
        )
      )
    ),
    list(
      input = c("$data foo.csv &  ", "(FE) rec 3"),
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "), elem_ampersand(),
          elem_whitespace("  "), elem_linebreak(),
          2L, elem_whitespace(" "), 3L, elem_linebreak()
        ),
        options = list(
          filename = option_pos$new("filename", value = "foo.csv"),
          format = option_pos$new("format", value = "(FE)"),
          records = option_value$new("records", "rec", value = "3", sep = " ")
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
        template = list(
          "record_name", elem_whitespace("    "),
          1L, elem_whitespace(" "),
          elem_comment("; a comment"), elem_linebreak(),
          2L, elem_whitespace(" "), 3L, elem_linebreak(),
          elem_whitespace("\t"), elem_comment("; trailing comment"),
          elem_linebreak()
        ),
        options = list(
          filename = option_pos$new("filename", value = "foo.csv"),
          ignore = option_value$new(
            "ignore",
            name_raw = "IGNORE", value = "( inside paren )", sep = ""
          ),
          norewind = option_flag$new(
            "norewind",
            name_raw = "norew", value = TRUE
          )
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
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_linebreak(), elem_comment("; comment"), elem_linebreak(),
          2L, elem_whitespace(" "), 3L, elem_linebreak()
        ),
        options = list(
          filename = option_pos$new("filename", value = "foo.csv"),
          ignore = option_value$new(
            "ignore",
            name_raw = "IGNORE", value = "C", sep = "="
          ),
          ignore = option_value$new(
            "ignore",
            name_raw = "IGNORE", value = "(foo)", sep = ""
          )
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
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "),
          2L, elem_linebreak()
        ),
        options = list(
          filename = option_pos$new("filename", value = "foo.csv"),
          ignore = option_value$new(
            "ignore",
            name_raw = "ign", value = "(foo ; bar\nbaz)", sep = " "
          )
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_data$new("data", "data", case$input)
    rec$parse()
    expect_identical(rec$template, case$want$template)
    expect_identical(rec$options, case$want$options)
    # Inputs and results match when rendered as string.
    expect_identical(
      format_from_template("data", rec$template, rec$options),
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
    expect_null(recs[[i]]$template)
    expect_null(recs[[i]]$options)
  }

  recs[[5]]$parse()

  expect_identical(
    recs[[2]]$template,
    list("record_name", elem_whitespace(" "), 1L, elem_linebreak())
  )
  expect_identical(
    recs[[2]]$options,
    list(filename = option_pos$new("filename", value = "foo.csv"))
  )

  expect_identical(
    recs[[4]]$template,
    list("record_name", elem_whitespace("\t"), 1L, elem_linebreak())
  )
  expect_identical(
    recs[[4]]$options,
    list(format = option_pos$new("format", value = "(F )"))
  )

  expect_identical(
    recs[[5]]$template,
    list("record_name", elem_whitespace(" "), 1L, elem_linebreak())
  )
  expect_identical(
    recs[[5]]$options,
    list(ignore = option_value$new(
      "ignore",
      name_raw = "ign", value = "C", sep = " "
    ))
  )

  expect_identical(
    format(res),
    paste0(paste0(lines, collapse = "\n"), "\n")
  )
})
