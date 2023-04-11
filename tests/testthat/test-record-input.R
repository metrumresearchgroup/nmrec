test_that("parse_input_record() works", {
  cases <- list(
    list(
      input = "$INPUT ID DOSE TIME CP=DV DROP",
      want = list(
        values = list(
          option_record_name$new("input", "INPUT"),
          elem_whitespace(" "),
          option_flag$new("ID", name_raw = "ID"),
          elem_whitespace(" "),
          option_flag$new("DOSE", name_raw = "DOSE"),
          elem_whitespace(" "),
          option_flag$new("TIME", name_raw = "TIME"),
          elem_whitespace(" "),
          option_value$new("CP", name_raw = "CP", value = "DV", sep = "="),
          elem_whitespace(" "),
          option_flag$new("DROP", name_raw = "DROP"),
          elem_linebreak()
        )
      )
    ),
    list(
      input = "$INPUT ID CP = DV",
      want = list(
        values = list(
          option_record_name$new("input", "INPUT"),
          elem_whitespace(" "),
          option_flag$new("ID", name_raw = "ID"),
          elem_whitespace(" "),
          option_value$new("CP", name_raw = "CP", value = "DV", sep = " = "),
          elem_linebreak()
        )
      )
    ),
    list(
      input = c(
        "$INPUT ID ; comment",
        "       CP=DV"
      ),
      want = list(
        values = list(
          option_record_name$new("input", "INPUT"),
          elem_whitespace(" "),
          option_flag$new("ID", name_raw = "ID"),
          elem_whitespace(" "),
          elem_comment("; comment"),
          elem_linebreak(),
          elem_whitespace("       "),
          option_value$new("CP", name_raw = "CP", value = "DV", sep = "="),
          elem_linebreak()
        )
      )
    )
  )

  for (case in cases) {
    rec <- record_input$new("input", "INPUT", case$input)
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

test_that("input records are combined", {
  lines <- c(
    "$prob",
    "$input ID",
    "$data foo.csv",
    "$input CP=DV"
  )

  ctl <- parse_ctl(lines)
  recs <- ctl$records

  for (i in c(2, 4)) {
    expect_null(recs[[i]]$values)
  }

  recs[[4]]$parse()

  expect_identical(
    recs[[2]]$values,
    list(
      option_record_name$new("input", "input"),
      elem_whitespace(" "),
      option_flag$new("ID", name_raw = "ID"),
      elem_linebreak()
    )
  )

  expect_identical(
    recs[[4]]$values,
    list(
      option_record_name$new("input", "input"),
      elem_whitespace(" "),
      option_value$new("CP", name_raw = "CP", value = "DV", sep = "="),
      elem_linebreak()
    )
  )

  expect_identical(
    format(ctl),
    paste0(paste0(lines, collapse = "\n"), "\n")
  )
})
