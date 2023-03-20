test_that("parse_data_record() aborts if filename is not on first line", {
  expect_error(parse_data_record("data", c("$data", "fn")),
    class = "nmrec_parse_error"
  )
})

test_that("parse_data_record() aborts on filename=*", {
  expect_error(parse_data_record("data", c("$data *")),
    class = "nmrec_unsupported"
  )
})

test_that("parse_data_record() aborts if format option lacks closing paren", {
  expect_error(parse_data_record("data", c("$data fn", "(FE")),
    class = "nmrec_parse_error"
  )
})

test_that("parse_data_record() works", {
  cases <- list(
    list(
      input = c("$data foo.csv"),
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          "filename", elem_linebreak()
        ),
        options = list(
          filename = option_pos$new("filename", value = "foo.csv")
        )
      )
    ),
    list(
      input = c("$data 'foo bar.csv'"),
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          "filename", elem_linebreak()
        ),
        options = list(
          filename = option_pos$new("filename", value = "'foo bar.csv'")
        )
      )
    ),
    list(
      input = c('$data "foo bar.csv"'),
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          "filename", elem_linebreak()
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
          "filename", elem_whitespace(" "), elem_ampersand(),
          elem_whitespace("  "), elem_linebreak(),
          "format", elem_whitespace(" "), "records", elem_linebreak()
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
          "filename", elem_whitespace(" "),
          elem_comment("; a comment"), elem_linebreak(),
          "ignore", elem_whitespace(" "), "norewind", elem_linebreak(),
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
    )
  )

  for (case in cases) {
    res <- parse_data_record("data", case$input)
    expect_identical(!!res, !!case$want)
    # Inputs and results match when rendered as string.
    expect_identical(
      format_from_template("data", res$template, res$options),
      paste0(
        paste0(!!case$input, collapse = "\n"),
        "\n"
      )
    )
  }
})
