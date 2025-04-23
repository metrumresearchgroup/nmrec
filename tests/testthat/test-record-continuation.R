test_that("continuation is parsed correctly", {
  cases <- list(
    list(
      record_type = "table",
      input = c(
        "$table ID format=1 &",
        "       firsto"
      ),
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "ID"),
          elem_whitespace(" "),
          option_value$new("format", name_raw = "format", value = "1"),
          elem_whitespace(" "),
          elem_ampersand(),
          elem_linebreak(),
          elem_whitespace("       "),
          option_flag$new("firstonly", name_raw = "firsto", value = TRUE),
          elem_linebreak()
        )
      )
    ),
    list(
      record_type = "table",
      input = c(
        "$table ID format=s1PE15.8:160& ",
        "       firsto"
      ),
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "ID"),
          elem_whitespace(" "),
          option_value$new(
            "format",
            name_raw = "format",
            value = "s1PE15.8:160"
          ),
          elem_ampersand(),
          elem_whitespace(" "),
          elem_linebreak(),
          elem_whitespace("       "),
          option_flag$new("firstonly", name_raw = "firsto", value = TRUE),
          elem_linebreak()
        )
      )
    ),
    list(
      record_type = "table",
      input = c(
        "$table ID format=s1PE15.8:160& ;",
        "       firsto"
      ),
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "ID"),
          elem_whitespace(" "),
          option_value$new(
            "format",
            name_raw = "format",
            value = "s1PE15.8:160&"
          ),
          elem_whitespace(" "),
          elem_comment(";"),
          elem_linebreak(),
          elem_whitespace("       "),
          option_flag$new("firstonly", name_raw = "firsto", value = TRUE),
          elem_linebreak()
        )
      )
    ),
    list(
      record_type = "table",
      input = c(
        "$table ID format=,1PE15.8:160& ;",
        "       firsto"
      ),
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "ID"),
          elem_whitespace(" "),
          option_value$new(
            "format",
            name_raw = "format",
            value = ",1PE15.8:160&"
          ),
          elem_whitespace(" "),
          elem_comment(";"),
          elem_linebreak(),
          elem_whitespace("       "),
          option_flag$new("firstonly", name_raw = "firsto", value = TRUE),
          elem_linebreak()
        )
      )
    ),
    list(
      record_type = "table",
      input = c(
        "$table ID file='foo bar'&",
        "       firsto"
      ),
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "ID"),
          elem_whitespace(" "),
          option_value$new(
            "file",
            name_raw = "file",
            value = "'foo bar'"
          ),
          elem_ampersand(),
          elem_linebreak(),
          elem_whitespace("       "),
          option_flag$new("firstonly", name_raw = "firsto", value = TRUE),
          elem_linebreak()
        )
      )
    ),
    list(
      record_type = "data",
      input = c(
        "$data foo.csv IGNORE(ID)& ;",
        " IGNORE=@"
      ),
      want = list(
        values = list(
          option_record_name$new("data", "data"),
          elem_whitespace(" "),
          option_pos$new("filename", value = "foo.csv"),
          elem_whitespace(" "),
          option_value$new(
            "ignore",
            name_raw = "IGNORE",
            value = "(ID)",
            sep = ""
          ),
          elem_ampersand(),
          elem_whitespace(" "),
          elem_comment(";"),
          elem_linebreak(),
          elem_whitespace(" "),
          option_value$new(
            "ignore",
            name_raw = "IGNORE",
            value = "@",
            sep = "="
          ),
          elem_linebreak()
        )
      )
    )
  )

  for (case in cases) {
    rec <- make_record(case$record_type, case$record_type, case$input)
    rec$parse()
    expect_false(is.null(rec$values))
    expect_identical(rec$values, case$want$values)
    # Inputs and results match when rendered as string.
    expect_identical(
      format(rec),
      paste0(
        paste0(case$input, collapse = "\n"),
        "\n"
      )
    )
  }
})
