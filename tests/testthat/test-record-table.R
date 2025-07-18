test_that("table_option_types and table_option_names align", {
  expect_setequal(
    names(table_option_types),
    purrr::map_chr(
      ls(table_option_names),
      function(x) get(x, envir = table_option_names)
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
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "ID NUM"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c("$table ID NUM ; comment", " PRED  noa,rformat=1"),
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "ID NUM ; comment\n PRED"),
          elem_whitespace("  "),
          option_flag$new("noappend", name_raw = "noa", value = TRUE),
          elem_comma(),
          option_value$new("rformat", name_raw = "rformat", value = "1"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$table a EXCLUDE_BY b lasto by c d nop by e",
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "a"),
          elem_whitespace(" "),
          option_pos$new("exclude_by", value = "EXCLUDE_BY b"),
          elem_whitespace(" "),
          option_flag$new("lastonly", name_raw = "lasto", value = TRUE),
          elem_whitespace(" "),
          option_pos$new("by", value = "by c d"),
          elem_whitespace(" "),
          option_flag$new("noprint", name_raw = "nop", value = TRUE),
          elem_whitespace(" "),
          option_pos$new("by", value = "by e"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$table id wres",
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "id wres"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$table wres",
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_flag$new("wreschol", name_raw = "wres", value = TRUE),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$table id noapp wres",
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "id"),
          elem_whitespace(" "),
          option_flag$new("noappend", name_raw = "noapp", value = TRUE),
          elem_whitespace(" "),
          option_flag$new("wreschol", name_raw = "wres", value = TRUE),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$table id noapp interp=1",
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "id"),
          elem_whitespace(" "),
          option_flag$new("noappend", name_raw = "noapp", value = TRUE),
          elem_whitespace(" "),
          option_value$new("interptype", name_raw = "interp", value = "1"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$table id noapp file='quo;ted'",
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "id"),
          elem_whitespace(" "),
          option_flag$new("noappend", name_raw = "noapp", value = TRUE),
          elem_whitespace(" "),
          option_value$new("file", name_raw = "file", value = "'quo;ted'"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$table id file'foo'",
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "id"),
          elem_whitespace(" "),
          option_value$new(
            "file",
            name_raw = "file",
            sep = "",
            value = "'foo'"
          ),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$table num format=,1PE11.4",
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "num"),
          elem_whitespace(" "),
          option_value$new("format", name_raw = "format", value = ",1PE11.4"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$table num format,1PE11.4 file=a",
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "num"),
          elem_whitespace(" "),
          option_value$new(
            "format",
            name_raw = "format",
            sep = "",
            value = ",1PE11.4"
          ),
          elem_whitespace(" "),
          option_value$new("file", name_raw = "file", value = "a"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$table num format ,1PE11.4 file=a",
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "num"),
          elem_whitespace(" "),
          option_value$new(
            "format",
            name_raw = "format",
            sep = " ",
            value = ",1PE11.4"
          ),
          elem_whitespace(" "),
          option_value$new("file", name_raw = "file", value = "a"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$table num format=,1PE15.8:160",
      want = list(
        values = list(
          option_record_name$new("table", "table"),
          elem_whitespace(" "),
          option_pos$new("list1", value = "num"),
          elem_whitespace(" "),
          option_value$new(
            "format",
            name_raw = "format",
            sep = "=",
            value = ",1PE15.8:160"
          ),
          elem_linebreak()
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_table$new("table", "table", case$input)
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
