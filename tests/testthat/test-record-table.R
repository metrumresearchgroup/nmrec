test_that("table_option_types and table_option_names align", {
  expect_setequal(
    names(table_option_types),
    purrr::map_chr(
      ls(table_option_names),
      ~ get(.x, envir = table_option_names)
    )
  )
})

test_that("parse_table_record() aborts if list1 repeated", {
  rec <- record_table$new("table", "tab", "$tab id num noappend foo bar")
  expect_error(rec$parse(), class = "nmrec_parse_error")
})

test_that("parse_table_record() aborts if option closing trailing quote", {
  rec <- record_table$new("table", "tab", "$tab id file='noclose")
  expect_error(rec$parse(), class = "nmrec_parse_error")
})

test_that("parse_table_record() works", {
  cases <- list(
    list(
      input = "$table ID NUM",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          list1 = option_pos$new("list1", value = "ID NUM")
        )
      )
    ),
    list(
      input = c("$table ID NUM ; comment", " PRED  noa,rformat=1"),
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace("  "), 2L, elem_comma(), 3L, elem_linebreak()
        ),
        options = list(
          list1 = option_pos$new("list1", value = "ID NUM ; comment\n PRED"),
          noappend = option_flag$new(
            "noappend",
            name_raw = "noa", value = TRUE
          ),
          rformat = option_value$new(
            "rformat",
            name_raw = "rformat", value = "1"
          )
        )
      )
    ),
    list(
      input = "$table a EXCLUDE_BY b lasto by c d nop by e",
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
          list1 = option_pos$new("list1", value = "a"),
          exclude_by = option_pos$new(
            "exclude_by",
            value = "EXCLUDE_BY b"
          ),
          lastonly = option_flag$new(
            "lastonly",
            name_raw = "lasto", value = TRUE
          ),
          by = option_pos$new("by", value = "by c d"),
          noprint = option_flag$new(
            "noprint",
            name_raw = "nop", value = TRUE
          ),
          by = option_pos$new("by", value = "by e")
        )
      )
    ),
    list(
      input = "$table id wres",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          list1 = option_pos$new("list1", value = "id wres")
        )
      )
    ),
    list(
      input = "$table wres",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "), 1L, elem_linebreak()
        ),
        options = list(
          wreschol = option_flag$new(
            "wreschol",
            name_raw = "wres", value = TRUE
          )
        )
      )
    ),
    list(
      input = "$table id noapp wres",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "),
          2L, elem_whitespace(" "),
          3L, elem_linebreak()
        ),
        options = list(
          list1 = option_pos$new("list1", value = "id"),
          noappend = option_flag$new(
            "noappend",
            name_raw = "noapp", value = TRUE
          ),
          wreschol = option_flag$new(
            "wreschol",
            name_raw = "wres", value = TRUE
          )
        )
      )
    ),
    list(
      input = "$table id noapp file='quo;ted'",
      want = list(
        template = list(
          "record_name", elem_whitespace(" "),
          1L, elem_whitespace(" "),
          2L, elem_whitespace(" "),
          3L, elem_linebreak()
        ),
        options = list(
          list1 = option_pos$new("list1", value = "id"),
          noappend = option_flag$new(
            "noappend",
            name_raw = "noapp", value = TRUE
          ),
          file = option_value$new(
            "file",
            name_raw = "file", value = "'quo;ted'"
          )
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_table$new("table", "table", case$input)
    rec$parse()
    expect_identical(rec$template, case$want$template)
    expect_identical(rec$options, case$want$options)
    # Inputs and results match when rendered as string.
    expect_identical(
      format_from_template("table", rec$template, rec$options),
      paste0(
        paste0(case$input, collapse = "\n"),
        "\n"
      )
    )
  }
})
